#include <charconv>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <system_error>

#include "fmt/core.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

std::string read_file(std::filesystem::path const& p) {
  auto in = std::ifstream{p, std::ios::binary};
  if (!in) {
    throw std::runtime_error{fmt::format("cannot read {}", p.string())};
  }
  auto ss = std::stringstream{};
  ss << in.rdbuf();
  return ss.str();
}

void write_file(std::filesystem::path const& p, std::string_view content) {
  auto const tmp = std::filesystem::path{p.string() + ".part"};
  {
    auto out = std::ofstream{tmp, std::ios::binary | std::ios::trunc};
    if (!out) {
      throw std::runtime_error{fmt::format("cannot write {}", tmp.string())};
    }
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
    if (!out) {
      throw std::runtime_error{fmt::format("write failed: {}", tmp.string())};
    }
  }
  std::filesystem::rename(tmp, p);
}

std::vector<std::string_view> split_lines(std::string_view s) {
  auto lines = std::vector<std::string_view>{};
  while (!s.empty()) {
    auto const end = s.find('\n');
    auto l = s.substr(0, end);
    if (!l.empty() && l.back() == '\r') {
      l.remove_suffix(1);
    }
    if (!l.empty()) {
      lines.push_back(l);
    }
    if (end == std::string_view::npos) {
      break;
    }
    s.remove_prefix(end + 1);
  }
  return lines;
}

// Every field is an identifier or a number: no quoting.
std::vector<std::string_view> split_fields(std::string_view l) {
  auto fields = std::vector<std::string_view>{};
  while (true) {
    auto const end = l.find(',');
    fields.push_back(l.substr(0, end));
    if (end == std::string_view::npos) {
      return fields;
    }
    l.remove_prefix(end + 1);
  }
}

double parse_double(std::string_view s, std::string_view what) {
  auto v = 0.0;
  auto const [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), v);
  if (ec != std::errc{} || ptr != s.data() + s.size()) {
    throw std::runtime_error{fmt::format("bad {}: '{}'", what, s)};
  }
  return v;
}

std::uint64_t fnv1a(std::string_view s) {
  auto h = std::uint64_t{14695981039346656037ULL};
  for (auto const c : s) {
    h ^= static_cast<std::uint8_t>(c);
    h *= 1099511628211ULL;
  }
  return h;
}

std::string hex64(std::uint64_t v) { return fmt::format("{:016x}", v); }

}  // namespace obehy::shapes
