#include <stdexcept>

#include "fmt/core.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

namespace {

constexpr auto kHeader = std::string_view{"pattern_id,class,seq,lat,lon,waypoint"};

bool valid_id(std::string_view id) {
  if (id.empty()) {
    return false;
  }
  for (auto const c : id) {
    auto const ok = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                    (c >= '0' && c <= '9') || c == ':' || c == '_' || c == '-';
    if (!ok) {
      return false;
    }
  }
  return true;
}

}  // namespace

shape_class parse_class(std::string_view c) {
  if (c == "bus") {
    return shape_class::kBus;
  }
  if (c == "rail") {
    return shape_class::kRail;
  }
  if (c == "ferry") {
    return shape_class::kFerry;
  }
  return shape_class::kUnsupported;
}

// patterns.csv: one row per match point, grouped by pattern, `seq` counting from 0. Patterns
// must appear in strictly increasing `pattern_id` order (byte order).
std::vector<pattern> read_patterns(std::filesystem::path const& p) {
  auto const content = read_file(p);
  auto const lines = split_lines(content);
  if (lines.empty() || lines.front() != kHeader) {
    throw std::runtime_error{
        fmt::format("{}: header must be '{}'", p.string(), kHeader)};
  }
  auto patterns = std::vector<pattern>{};
  for (auto n = 1U; n < lines.size(); ++n) {
    auto const fail = [&](std::string_view why) {
      return std::runtime_error{
          fmt::format("{}:{}: {}", p.string(), n + 1U, why)};
    };
    auto const f = split_fields(lines[n]);
    if (f.size() != 6U) {
      throw fail("expected 6 fields");
    }
    if (!valid_id(f[0])) {
      throw fail("bad pattern_id");
    }
    if (patterns.empty() || patterns.back().id_ != f[0]) {
      if (!patterns.empty() && !(patterns.back().id_ < f[0])) {
        throw fail("patterns not in increasing pattern_id order");
      }
      patterns.push_back(pattern{std::string{f[0]}, std::string{f[1]}, {}});
    }
    auto& pat = patterns.back();
    if (pat.class_name_ != f[1]) {
      throw fail("class changes within a pattern");
    }
    if (f[2] != std::to_string(pat.points_.size())) {
      throw fail("seq must count from 0 without gaps");
    }
    auto const lat = parse_double(f[3], "lat");
    auto const lon = parse_double(f[4], "lon");
    if (lat < -90.0 || lat > 90.0 || lon < -180.0 || lon > 180.0) {
      throw fail("coordinate out of range");
    }
    if (f[5] != "0" && f[5] != "1") {
      throw fail("waypoint must be 0 or 1");
    }
    pat.points_.emplace_back(lat, lon);
  }
  return patterns;
}

}  // namespace obehy::shapes
