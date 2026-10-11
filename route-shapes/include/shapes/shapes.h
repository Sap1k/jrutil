#pragma once

// obehy-motis-shapes: map-matches JrUtil stop patterns onto OSM with osr (MOTIS's router) and
// writes route shapes with per-match-point offsets (BASE_PLAN.md section 17).

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <map>
#include <string>
#include <string_view>
#include <vector>

#include "geo/latlng.h"

namespace osr {
struct ways;
struct lookup;
}  // namespace osr

namespace obehy::shapes {

// Bumped whenever the output bytes for the same inputs would change.
constexpr auto kFormatVersion = 1;

enum class shape_class : std::uint8_t { kBus, kRail, kFerry, kUnsupported };

shape_class parse_class(std::string_view);

enum class status : std::uint8_t {
  kRouted,
  kPartialBeeline,
  kFailed,
  kUnsupported,
  kTooFewPoints
};

std::string_view to_str(status);
status parse_status(std::string_view);

struct pattern {
  std::string id_;
  std::string class_name_;
  std::vector<geo::latlng> points_;
};

// One pattern's output, already formatted: cached rows are copied verbatim, so cold and warm
// runs write identical bytes.
struct result {
  std::string id_;
  std::string class_name_;
  status status_{status::kFailed};
  std::string status_row_;
  std::string shape_rows_;
  std::string offset_rows_;
};

// --- csv.cc ---
std::string read_file(std::filesystem::path const&);
void write_file(std::filesystem::path const&, std::string_view);
std::vector<std::string_view> split_lines(std::string_view);
std::vector<std::string_view> split_fields(std::string_view);
double parse_double(std::string_view, std::string_view what);
std::uint64_t fnv1a(std::string_view);
std::string hex64(std::uint64_t);

// --- geometry.cc ---
struct line {
  std::vector<geo::latlng> points_;
  std::vector<std::size_t> offsets_;  // point index of every match point, non-decreasing
};

// Simplifies each span between consecutive offsets separately (Douglas-Peucker, metres), so
// every offset stays a vertex; rounds to 1e-7 degrees and drops consecutive duplicates.
line finish_line(line const& raw, double tolerance_m);
std::vector<double> cumulative_m(std::vector<geo::latlng> const&);
std::string format_coord(double);
std::string format_m(double);

// --- patterns.cc ---
std::vector<pattern> read_patterns(std::filesystem::path const&);

// --- route.cc ---
result route_pattern(osr::ways const&,
                     osr::lookup const&,
                     pattern const&,
                     double simplify_m);
result unrouted(pattern const&, status);

// --- cache.cc ---
std::map<std::string, result> load_cache(std::filesystem::path const& dir,
                                         std::string const& key);
void save_cache(std::filesystem::path const& dir,
                std::string const& key,
                std::vector<result> const&);

// --- run.cc ---
struct route_options {
  std::filesystem::path graph_;
  std::filesystem::path patterns_;
  std::filesystem::path out_;
  std::filesystem::path cache_;  // empty: no cache
  unsigned threads_{0U};         // 0: hardware concurrency
  double simplify_m_{2.0};
};

std::string_view osr_commit();
void extract_graph(std::filesystem::path const& osm,
                   std::filesystem::path const& graph,
                   std::string const& key);
void route_all(route_options const&);

}  // namespace obehy::shapes
