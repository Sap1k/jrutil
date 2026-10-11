#include <algorithm>
#include <array>
#include <atomic>
#include <map>
#include <chrono>
#include <iostream>
#include <stdexcept>
#include <thread>

#include "fmt/core.h"

#include "cista/mmap.h"

#include "osr/extract/extract.h"
#include "osr/lookup.h"
#include "osr/ways.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

namespace {

constexpr auto kGraphKeyFile = "obehy-graph.txt";

constexpr auto kStatuses =
    std::array{status::kRouted, status::kPartialBeeline, status::kFailed,
               status::kUnsupported, status::kTooFewPoints};

std::string graph_key(std::filesystem::path const& graph) {
  auto const f = graph / kGraphKeyFile;
  if (!std::filesystem::is_regular_file(f)) {
    throw std::runtime_error{fmt::format(
        "{} is not a graph written by `obehy-motis-shapes extract`",
        graph.string())};
  }
  auto const content = read_file(f);
  auto const lines = split_lines(content);
  if (lines.size() != 2U || lines[1] != osr_commit()) {
    throw std::runtime_error{fmt::format(
        "graph {} was extracted by another osr build", graph.string())};
  }
  return std::string{lines[0]};
}

std::string diagnostics_json(std::vector<result> const& results) {
  auto counts = std::map<std::string, std::array<std::size_t, kStatuses.size()>>{};
  for (auto const& r : results) {
    auto& c = counts[r.class_name_];
    for (auto i = 0U; i < kStatuses.size(); ++i) {
      if (kStatuses[i] == r.status_) {
        ++c[i];
      }
    }
  }
  auto out = fmt::format("{{\n  \"format\": {},\n  \"patterns\": {},\n  \"by_class\": {{",
                         kFormatVersion, results.size());
  auto first_class = true;
  for (auto const& [cls, c] : counts) {
    out += fmt::format("{}\n    \"{}\": {{", first_class ? "" : ",", cls);
    for (auto i = 0U; i < kStatuses.size(); ++i) {
      out += fmt::format("{}\"{}\": {}", i == 0U ? "" : ", ",
                         to_str(kStatuses[i]), c[i]);
    }
    out += "}";
    first_class = false;
  }
  out += "\n  }\n}\n";
  return out;
}

}  // namespace

std::string_view osr_commit() { return OBEHY_OSR_COMMIT; }

void extract_graph(std::filesystem::path const& osm,
                   std::filesystem::path const& graph,
                   std::string const& key) {
  if (!std::filesystem::is_regular_file(osm)) {
    throw std::runtime_error{fmt::format("{} not found", osm.string())};
  }
  if (key.empty() || key.find('\n') != std::string::npos) {
    throw std::runtime_error{"--key must be one non-empty line"};
  }
  std::filesystem::create_directories(graph);
  std::filesystem::remove(graph / kGraphKeyFile);
  osr::extract(false, osm, graph, {});
  write_file(graph / kGraphKeyFile, fmt::format("{}\n{}\n", key, osr_commit()));
}

void route_all(route_options const& o) {
  auto const started = std::chrono::steady_clock::now();
  auto const gkey = graph_key(o.graph_);
  auto const patterns = read_patterns(o.patterns_);
  auto const key =
      fmt::format("format={};osr={};graph={};simplify_m={}", kFormatVersion,
                  osr_commit(), gkey, format_m(o.simplify_m_));

  auto cached = o.cache_.empty() ? std::map<std::string, result>{}
                                 : load_cache(o.cache_, key);
  auto results = std::vector<result>(patterns.size());
  auto todo = std::vector<std::size_t>{};
  for (auto i = 0U; i < patterns.size(); ++i) {
    auto const it = cached.find(patterns[i].id_);
    if (it != cached.end() && it->second.class_name_ == patterns[i].class_name_) {
      results[i] = std::move(it->second);
    } else {
      todo.push_back(i);
    }
  }

  if (!todo.empty()) {
    auto const w = osr::ways{o.graph_, cista::mmap::protection::READ};
    auto const l = osr::lookup{w, o.graph_, cista::mmap::protection::READ};
    auto next = std::atomic_size_t{0U};
    auto const n_threads =
        o.threads_ != 0U ? o.threads_
                         : std::max(1U, std::thread::hardware_concurrency());
    auto workers = std::vector<std::jthread>{};
    for (auto t = 0U; t < n_threads; ++t) {
      workers.emplace_back([&] {
        for (auto j = next++; j < todo.size(); j = next++) {
          auto const i = todo[j];
          results[i] = route_pattern(w, l, patterns[i], o.simplify_m_);
        }
      });
    }
    workers.clear();
  }

  std::filesystem::create_directories(o.out_);
  auto status = std::string{"pattern_id,class,status,points,routed_m,beeline_m\n"};
  auto shapes = std::string{"pattern_id,seq,lat,lon,dist_m\n"};
  auto offsets = std::string{"pattern_id,match_seq,point_index,dist_m\n"};
  for (auto const& r : results) {
    status += r.status_row_;
    shapes += r.shape_rows_;
    offsets += r.offset_rows_;
  }
  write_file(o.out_ / "pattern_status.csv", status);
  write_file(o.out_ / "shapes.csv", shapes);
  write_file(o.out_ / "offsets.csv", offsets);
  write_file(o.out_ / "diagnostics.json", diagnostics_json(results));
  write_file(o.out_ / "manifest.json",
             fmt::format("{{\n  \"tool\": \"obehy-motis-shapes\",\n"
                         "  \"format\": {},\n  \"osr_commit\": \"{}\",\n"
                         "  \"graph_key\": \"{}\",\n  \"simplify_m\": {},\n"
                         "  \"patterns_fnv1a\": \"{}\"\n}}\n",
                         kFormatVersion, osr_commit(), gkey,
                         format_m(o.simplify_m_),
                         hex64(fnv1a(read_file(o.patterns_)))));
  if (!o.cache_.empty()) {
    save_cache(o.cache_, key, results);
  }

  auto const secs = std::chrono::duration<double>(
                        std::chrono::steady_clock::now() - started)
                        .count();
  std::cerr << fmt::format("{} patterns: {} routed now, {} from cache, {:.1f} s\n",
                           patterns.size(), todo.size(),
                           patterns.size() - todo.size(), secs);
}

}  // namespace obehy::shapes
