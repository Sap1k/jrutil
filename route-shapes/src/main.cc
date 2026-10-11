#include <algorithm>
#include <charconv>
#include <iostream>
#include <map>
#include <stdexcept>
#include <string>

#include "fmt/core.h"

#include "shapes/shapes.h"

namespace s = obehy::shapes;

namespace {

constexpr auto kUsage =
    "usage:\n"
    "  obehy-motis-shapes extract --osm <routing.osm.pbf> --graph <dir> --key <text>\n"
    "  obehy-motis-shapes route --graph <dir> --patterns <patterns.csv> --out <dir>\n"
    "                           [--cache <dir>] [--threads auto|N] [--simplify-m M]\n"
    "  obehy-motis-shapes version\n";

struct usage_error : std::runtime_error {
  using std::runtime_error::runtime_error;
};

std::map<std::string, std::string> options(int const argc,
                                           char const* const* argv,
                                           std::initializer_list<std::string> allowed) {
  auto o = std::map<std::string, std::string>{};
  for (auto i = 2; i < argc; i += 2) {
    auto const name = std::string{argv[i]};
    if (std::find(allowed.begin(), allowed.end(), name) == allowed.end()) {
      throw usage_error{fmt::format("unknown option {}", name)};
    }
    if (i + 1 >= argc) {
      throw usage_error{fmt::format("{} needs a value", name)};
    }
    o[name] = argv[i + 1];
  }
  return o;
}

std::string required(std::map<std::string, std::string> const& o,
                     std::string const& name) {
  auto const it = o.find(name);
  if (it == o.end()) {
    throw usage_error{fmt::format("{} is required", name)};
  }
  return it->second;
}

}  // namespace

int main(int argc, char const** argv) {
  try {
    auto const command = argc > 1 ? std::string{argv[1]} : std::string{};
    if (command == "version") {
      std::cout << fmt::format("obehy-motis-shapes format {} osr {}\n",
                               s::kFormatVersion, s::osr_commit());
      return 0;
    }
    if (command == "extract") {
      auto const o = options(argc, argv, {"--osm", "--graph", "--key"});
      s::extract_graph(required(o, "--osm"), required(o, "--graph"),
                       required(o, "--key"));
      return 0;
    }
    if (command == "route") {
      auto const o = options(argc, argv,
                             {"--graph", "--patterns", "--out", "--cache",
                              "--threads", "--simplify-m"});
      auto r = s::route_options{.graph_ = required(o, "--graph"),
                                .patterns_ = required(o, "--patterns"),
                                .out_ = required(o, "--out")};
      if (o.contains("--cache")) {
        r.cache_ = o.at("--cache");
      }
      if (o.contains("--threads") && o.at("--threads") != "auto") {
        auto const& t = o.at("--threads");
        auto const [ptr, ec] = std::from_chars(t.data(), t.data() + t.size(), r.threads_);
        if (ec != std::errc{} || ptr != t.data() + t.size() || r.threads_ == 0U) {
          throw usage_error{"--threads must be auto or a positive integer"};
        }
      }
      if (o.contains("--simplify-m")) {
        r.simplify_m_ = s::parse_double(o.at("--simplify-m"), "--simplify-m");
        if (r.simplify_m_ < 0.0) {
          throw usage_error{"--simplify-m must not be negative"};
        }
      }
      s::route_all(r);
      return 0;
    }
    throw usage_error{command.empty() ? "no command" : "unknown command " + command};
  } catch (usage_error const& e) {
    std::cerr << e.what() << "\n" << kUsage;
    return 2;
  } catch (std::exception const& e) {
    std::cerr << "error: " << e.what() << "\n";
    return 1;
  }
}
