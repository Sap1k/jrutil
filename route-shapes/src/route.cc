#include <exception>
#include <iostream>
#include <optional>

#include "fmt/core.h"

#include "osr/lookup.h"
#include "osr/routing/map_matching.h"
#include "osr/routing/parameters.h"
#include "osr/routing/profile.h"
#include "osr/ways.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

namespace {

// The class → profile mapping of MOTIS's route_shapes (get_profile in src/route_shapes.cc):
// bus and coach on the bus profile, tram, metro and all rail on the railway profile.
std::optional<osr::search_profile> profile_of(shape_class const c) {
  switch (c) {
    case shape_class::kBus: return osr::search_profile::kBus;
    case shape_class::kRail: return osr::search_profile::kRailway;
    case shape_class::kFerry: return osr::search_profile::kFerry;
    case shape_class::kUnsupported: return std::nullopt;
  }
  return std::nullopt;
}

std::size_t distinct_points(std::vector<geo::latlng> const& points) {
  auto n = std::size_t{0U};
  for (auto i = 0U; i < points.size(); ++i) {
    if (i == 0U || points[i].lat() != points[i - 1U].lat() ||
        points[i].lng() != points[i - 1U].lng()) {
      ++n;
    }
  }
  return n;
}

double polyline_m(geo::polyline const& p) {
  auto total = 0.0;
  for (auto i = 1U; i < p.size(); ++i) {
    total += geo::distance(p[i - 1U], p[i]);
  }
  return total;
}

std::string status_row(pattern const& p,
                        status const s,
                        std::size_t const points,
                        double const routed_m,
                        double const beeline_m) {
  return fmt::format("{},{},{},{},{},{}\n", p.id_, p.class_name_, to_str(s),
                     points, format_m(routed_m), format_m(beeline_m));
}

}  // namespace

std::string_view to_str(status const s) {
  switch (s) {
    case status::kRouted: return "routed";
    case status::kPartialBeeline: return "partial_beeline";
    case status::kFailed: return "failed";
    case status::kUnsupported: return "unsupported";
    case status::kTooFewPoints: return "too_few_points";
  }
  return "failed";
}

status parse_status(std::string_view s) {
  for (auto const c : {status::kRouted, status::kPartialBeeline, status::kFailed,
                       status::kUnsupported, status::kTooFewPoints}) {
    if (to_str(c) == s) {
      return c;
    }
  }
  throw std::runtime_error{fmt::format("unknown status '{}'", s)};
}

result unrouted(pattern const& p, status const s) {
  return result{.id_ = p.id_,
                .class_name_ = p.class_name_,
                .status_ = s,
                .status_row_ = status_row(p, s, 0U, 0.0, 0.0),
                .shape_rows_ = {},
                .offset_rows_ = {}};
}

result route_pattern(osr::ways const& w,
                     osr::lookup const& l,
                     pattern const& p,
                     double const simplify_m) {
  auto const profile = profile_of(parse_class(p.class_name_));
  if (!profile.has_value()) {
    return unrouted(p, status::kUnsupported);
  }
  if (distinct_points(p.points_) < 2U) {
    return unrouted(p, status::kTooFewPoints);
  }

  auto match_points = std::vector<osr::location>{};
  match_points.reserve(p.points_.size());
  for (auto const& pos : p.points_) {
    match_points.push_back(osr::location{pos, osr::kNoLevel});
  }

  auto matched = osr::matched_route{};
  try {
    matched = osr::map_match(w, l, *profile, osr::get_parameters(*profile),
                             match_points);
  } catch (std::exception const& e) {
    std::cerr << fmt::format("pattern {}: map matching failed: {}\n", p.id_,
                             e.what());
    return unrouted(p, status::kFailed);
  }
  if (matched.segment_offsets_.size() != match_points.size()) {
    std::cerr << fmt::format("pattern {}: {} segment offsets for {} points\n",
                             p.id_, matched.segment_offsets_.size(),
                             match_points.size());
    return unrouted(p, status::kFailed);
  }

  // Concatenation as in MOTIS's route_shape (src/route_shapes.cc): the path segments of match
  // point i run from segment_offsets_[i] to the next offset (the last to the path's end).
  auto raw = line{};
  raw.offsets_.push_back(0U);
  auto const& segments = matched.path_.segments_;
  for (auto i = 0U; i + 1U < match_points.size(); ++i) {
    auto const start = matched.segment_offsets_[i];
    auto const end = i + 1U < match_points.size() - 1U
                         ? matched.segment_offsets_[i + 1U]
                         : segments.size();
    for (auto s = start; s < end; ++s) {
      auto const& poly = segments[s].polyline_;
      if (poly.empty()) {
        continue;
      }
      auto first = poly.begin();
      if (!raw.points_.empty() && raw.points_.back() == *first) {
        ++first;
      }
      raw.points_.insert(raw.points_.end(), first, poly.end());
    }
    if (raw.points_.empty()) {
      raw.points_.push_back(p.points_.front());
    }
    raw.offsets_.push_back(raw.points_.size() - 1U);
  }

  // osr leaves `way_` unset on the straight segments it adds where matching found no route.
  auto routed_m = 0.0, beeline_m = 0.0;
  for (auto const& s : segments) {
    auto const m = s.dist_ != 0U ? static_cast<double>(s.dist_)
                                 : polyline_m(s.polyline_);
    (s.way_ == osr::way_idx_t::invalid() ? beeline_m : routed_m) += m;
  }

  auto const finished = finish_line(raw, simplify_m);
  if (finished.points_.size() < 2U) {
    return unrouted(p, status::kFailed);
  }
  auto const dist = cumulative_m(finished.points_);
  auto const s =
      matched.n_beelined_ > 0U ? status::kPartialBeeline : status::kRouted;

  auto r = result{.id_ = p.id_,
                  .class_name_ = p.class_name_,
                  .status_ = s,
                  .status_row_ = status_row(p, s, finished.points_.size(),
                                            routed_m, beeline_m),
                  .shape_rows_ = {},
                  .offset_rows_ = {}};
  for (auto i = 0U; i < finished.points_.size(); ++i) {
    r.shape_rows_ += fmt::format(
        "{},{},{},{},{}\n", p.id_, i, format_coord(finished.points_[i].lat()),
        format_coord(finished.points_[i].lng()), format_m(dist[i]));
  }
  for (auto i = 0U; i < finished.offsets_.size(); ++i) {
    auto const idx = finished.offsets_[i];
    r.offset_rows_ += fmt::format("{},{},{},{}\n", p.id_, i, idx,
                                  format_m(dist[idx]));
  }
  return r;
}

}  // namespace obehy::shapes
