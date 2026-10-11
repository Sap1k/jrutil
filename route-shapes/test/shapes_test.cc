#include <cmath>
#include <map>
#include <string>
#include <vector>

#include "gtest/gtest.h"

#include "shapes/shapes.h"

namespace fs = std::filesystem;
namespace s = obehy::shapes;

namespace {

// Bus: the L road, stops at both ends and at the corner's far side (no stop at the corner).
// Loop: out along the L and back. Rail: m1 → m3 through the junction, with a waypoint on the
// north branch so the line cannot be cut. Beeline: a stop 4 km from any road.
constexpr auto kPatterns =
    "pattern_id,class,seq,lat,lon,waypoint\n"
    "a-bus,bus,0,50.0000500,14.0002000,0\n"
    "a-bus,bus,1,50.0098000,14.0100500,0\n"
    "b-loop,bus,0,50.0000500,14.0002000,0\n"
    "b-loop,bus,1,50.0098000,14.0100500,0\n"
    "b-loop,bus,2,50.0000500,14.0002000,0\n"
    "c-rail,rail,0,50.0200500,14.0005000,0\n"
    "c-rail,rail,1,50.0250500,14.0150000,1\n"
    "c-rail,rail,2,50.0299500,14.0199000,0\n"
    "d-ferry,ferry,0,50.0400500,14.0005000,0\n"
    "d-ferry,ferry,1,50.0400500,14.0195000,0\n"
    "e-beeline,bus,0,50.0000500,14.0002000,0\n"
    "e-beeline,bus,1,50.0600000,14.0500000,0\n"
    "f-cable,cable,0,50.0000500,14.0002000,0\n"
    "f-cable,cable,1,50.0098000,14.0100500,0\n"
    "g-same,bus,0,50.0000500,14.0002000,0\n"
    "g-same,bus,1,50.0000500,14.0002000,0\n";

struct shape_point {
  double lat_, lon_, dist_;
};

struct output {
  std::map<std::string, std::vector<std::string>> status_;
  std::map<std::string, std::vector<shape_point>> shapes_;
  std::map<std::string, std::vector<std::size_t>> offsets_;
};

fs::path temp_root() {
  static auto const root = [] {
    auto const r = fs::temp_directory_path() / "obehy-motis-shapes-test";
    fs::remove_all(r);
    fs::create_directories(r);
    s::extract_graph(fs::path{OBEHY_TEST_DIR} / "fixture.osm.pbf", r / "graph",
                     "fixture");
    s::write_file(r / "patterns.csv", kPatterns);
    return r;
  }();
  return root;
}

fs::path run(std::string const& name, unsigned threads, bool cache) {
  auto const out = temp_root() / name;
  s::route_all(s::route_options{.graph_ = temp_root() / "graph",
                                .patterns_ = temp_root() / "patterns.csv",
                                .out_ = out,
                                .cache_ = cache ? temp_root() / "cache" : fs::path{},
                                .threads_ = threads,
                                .simplify_m_ = 2.0});
  return out;
}

output read(fs::path const& dir) {
  auto o = output{};
  auto files = std::map<std::string, std::string>{};  // owns the bytes the row views point into
  auto const rows = [&](char const* file) {
    auto lines = s::split_lines(files[file] = s::read_file(dir / file));
    lines.erase(lines.begin());  // header
    return lines;
  };
  for (auto const l : rows("pattern_status.csv")) {
    auto const f = s::split_fields(l);
    o.status_[std::string{f[0]}] = {f.begin(), f.end()};
  }
  for (auto const l : rows("shapes.csv")) {
    auto const f = s::split_fields(l);
    o.shapes_[std::string{f[0]}].push_back(
        {s::parse_double(f[2], "lat"), s::parse_double(f[3], "lon"),
         s::parse_double(f[4], "dist")});
  }
  for (auto const l : rows("offsets.csv")) {
    auto const f = s::split_fields(l);
    o.offsets_[std::string{f[0]}].push_back(
        static_cast<std::size_t>(s::parse_double(f[2], "index")));
  }
  return o;
}

bool passes_near(std::vector<shape_point> const& shape, double lat, double lon) {
  for (auto const& p : shape) {
    if (std::abs(p.lat_ - lat) < 0.0002 && std::abs(p.lon_ - lon) < 0.0002) {
      return true;
    }
  }
  return false;
}

std::string status_of(output const& o, std::string const& id) {
  return o.status_.at(id)[2];
}

}  // namespace

TEST(motis_shapes, bus_follows_the_road_around_the_corner) {
  auto const o = read(run("bus", 1U, false));
  EXPECT_EQ(status_of(o, "a-bus"), "routed");
  auto const& shape = o.shapes_.at("a-bus");
  EXPECT_TRUE(passes_near(shape, 50.0, 14.01));  // the corner, not a straight chord
  EXPECT_NEAR(shape.back().dist_, 1800.0, 150.0);  // ~ 700 m east + ~ 1090 m north
  EXPECT_EQ(o.offsets_.at("a-bus").front(), 0U);
  EXPECT_EQ(o.offsets_.at("a-bus").back(), shape.size() - 1U);
}

TEST(motis_shapes, loop_returns_and_offsets_stay_monotonic) {
  auto const o = read(run("loop", 1U, false));
  EXPECT_EQ(status_of(o, "b-loop"), "routed");
  auto const& off = o.offsets_.at("b-loop");
  ASSERT_EQ(off.size(), 3U);
  EXPECT_LT(off[0], off[1]);
  EXPECT_LT(off[1], off[2]);
  auto const& shape = o.shapes_.at("b-loop");
  EXPECT_NEAR(shape[off[2]].dist_, 2.0 * shape[off[1]].dist_, 100.0);
  for (auto i = 1U; i < shape.size(); ++i) {
    EXPECT_GE(shape[i].dist_, shape[i - 1U].dist_);
  }
}

TEST(motis_shapes, rail_takes_the_waypoint_branch) {
  auto const o = read(run("rail", 1U, false));
  EXPECT_EQ(status_of(o, "c-rail"), "routed");
  auto const& shape = o.shapes_.at("c-rail");
  EXPECT_TRUE(passes_near(shape, 50.02, 14.01));  // the junction
  EXPECT_FALSE(passes_near(shape, 50.015, 14.015));  // never the south-east branch
}

TEST(motis_shapes, ferry_unsupported_too_few_and_beeline) {
  auto const o = read(run("misc", 1U, false));
  EXPECT_EQ(status_of(o, "d-ferry"), "routed");
  EXPECT_EQ(status_of(o, "e-beeline"), "partial_beeline");
  EXPECT_GT(s::parse_double(o.status_.at("e-beeline")[5], "beeline_m"), 1000.0);
  EXPECT_EQ(status_of(o, "f-cable"), "unsupported");
  EXPECT_EQ(status_of(o, "g-same"), "too_few_points");
  EXPECT_FALSE(o.shapes_.contains("f-cable"));
  EXPECT_FALSE(o.shapes_.contains("g-same"));
}

TEST(motis_shapes, cold_warm_and_thread_counts_write_identical_bytes) {
  auto const cold = run("cold", 1U, true);
  auto const warm = run("warm", 4U, true);
  auto const parallel = run("parallel", 4U, false);
  for (auto const* file : {"pattern_status.csv", "shapes.csv", "offsets.csv",
                           "diagnostics.json", "manifest.json"}) {
    EXPECT_EQ(s::read_file(cold / file), s::read_file(warm / file)) << file;
    EXPECT_EQ(s::read_file(cold / file), s::read_file(parallel / file)) << file;
  }
}

TEST(motis_shapes, finish_line_keeps_every_offset_vertex) {
  // A straight line with an interior match point: simplification may drop the other points
  // but never the match point.
  auto raw = s::line{};
  for (auto i = 0; i <= 10; ++i) {
    raw.points_.emplace_back(50.0, 14.0 + 0.001 * i);
  }
  raw.offsets_ = {0U, 3U, 10U};
  auto const done = s::finish_line(raw, 5.0);
  ASSERT_EQ(done.points_.size(), 3U);
  EXPECT_EQ(done.offsets_, (std::vector<std::size_t>{0U, 1U, 2U}));
  EXPECT_DOUBLE_EQ(done.points_[1].lng(), 14.003);
}

TEST(motis_shapes, patterns_must_be_sorted_and_counted) {
  auto const bad = temp_root() / "bad.csv";
  s::write_file(bad,
                "pattern_id,class,seq,lat,lon,waypoint\n"
                "b,bus,0,50,14,0\nb,bus,1,50,14.1,0\na,bus,0,50,14,0\n");
  EXPECT_THROW(s::read_patterns(bad), std::runtime_error);
  s::write_file(bad, "pattern_id,class,seq,lat,lon,waypoint\na,bus,1,50,14,0\n");
  EXPECT_THROW(s::read_patterns(bad), std::runtime_error);
}
