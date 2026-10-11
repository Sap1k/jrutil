#include <algorithm>
#include <cmath>
#include <numbers>
#include <utility>

#include "fmt/core.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

namespace {

constexpr auto kEarthRadiusM = 6371000.0;

struct xy {
  double x_, y_;
};

// Local equirectangular projection around `origin`, good to well below a metre over the length
// of one stop-to-stop span.
xy project(geo::latlng const& origin, geo::latlng const& p) {
  constexpr auto kRad = std::numbers::pi / 180.0;
  return {(p.lng() - origin.lng()) * std::cos(origin.lat() * kRad) * kRad *
              kEarthRadiusM,
          (p.lat() - origin.lat()) * kRad * kEarthRadiusM};
}

double segment_distance(xy const& p, xy const& a, xy const& b) {
  auto const dx = b.x_ - a.x_, dy = b.y_ - a.y_;
  auto const len2 = dx * dx + dy * dy;
  auto t = len2 == 0.0 ? 0.0 : ((p.x_ - a.x_) * dx + (p.y_ - a.y_) * dy) / len2;
  t = std::clamp(t, 0.0, 1.0);
  auto const ex = a.x_ + t * dx - p.x_, ey = a.y_ + t * dy - p.y_;
  return std::sqrt(ex * ex + ey * ey);
}

// Douglas-Peucker over raw[from..to] (inclusive), marking kept points.
void mark_span(std::vector<geo::latlng> const& raw,
               std::size_t const from,
               std::size_t const to,
               double const tolerance_m,
               std::vector<bool>& keep) {
  keep[from] = true;
  keep[to] = true;
  if (to <= from + 1U) {
    return;
  }
  auto const origin = raw[from];
  auto stack = std::vector<std::pair<std::size_t, std::size_t>>{{from, to}};
  while (!stack.empty()) {
    auto const [a, b] = stack.back();
    stack.pop_back();
    auto const pa = project(origin, raw[a]), pb = project(origin, raw[b]);
    auto worst = 0.0;
    auto worst_idx = a;
    for (auto i = a + 1U; i < b; ++i) {
      auto const d = segment_distance(project(origin, raw[i]), pa, pb);
      if (d > worst) {
        worst = d;
        worst_idx = i;
      }
    }
    if (worst > tolerance_m) {
      keep[worst_idx] = true;
      stack.emplace_back(a, worst_idx);
      stack.emplace_back(worst_idx, b);
    }
  }
}

double round7(double const v) { return std::round(v * 1e7) / 1e7; }

}  // namespace

line finish_line(line const& raw, double const tolerance_m) {
  auto keep = std::vector<bool>(raw.points_.size(), false);
  for (auto i = 0U; i + 1U < raw.offsets_.size(); ++i) {
    mark_span(raw.points_, raw.offsets_[i], raw.offsets_[i + 1U], tolerance_m,
              keep);
  }
  if (raw.offsets_.size() == 1U) {
    keep[raw.offsets_.front()] = true;
  }

  auto out = line{};
  auto next_offset = 0U;
  for (auto i = 0U; i < raw.points_.size(); ++i) {
    if (keep[i]) {
      auto const p = geo::latlng{round7(raw.points_[i].lat()),
                                 round7(raw.points_[i].lng())};
      if (out.points_.empty() || out.points_.back().lat() != p.lat() ||
          out.points_.back().lng() != p.lng()) {
        out.points_.push_back(p);
      }
    }
    while (next_offset < raw.offsets_.size() && raw.offsets_[next_offset] == i) {
      out.offsets_.push_back(out.points_.size() - 1U);
      ++next_offset;
    }
  }
  return out;
}

std::vector<double> cumulative_m(std::vector<geo::latlng> const& points) {
  auto dist = std::vector<double>{};
  dist.reserve(points.size());
  auto total = 0.0;
  for (auto i = 0U; i < points.size(); ++i) {
    if (i != 0U) {
      total += geo::distance(points[i - 1U], points[i]);
    }
    dist.push_back(total);
  }
  return dist;
}

std::string format_coord(double const v) { return fmt::format("{:.7f}", v); }

std::string format_m(double const v) { return fmt::format("{:.1f}", v); }

}  // namespace obehy::shapes
