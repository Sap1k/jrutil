#include <iostream>
#include <stdexcept>

#include "fmt/core.h"

#include "shapes/shapes.h"

namespace obehy::shapes {

// The cache holds the formatted rows of the last run, under a directory named after the hash of
// everything that changes the output for the same pattern: format version, osr commit, routing
// graph and simplification. A pattern id hashes its class and match points, so a hit is exact.
// Only the patterns of the latest run are kept: the cache cannot grow without bound.

namespace {

std::filesystem::path entry_dir(std::filesystem::path const& dir,
                                std::string const& key) {
  return dir / hex64(fnv1a(key));
}

std::string_view row_id(std::string_view row) {
  return row.substr(0, row.find(','));
}

}  // namespace

std::map<std::string, result> load_cache(std::filesystem::path const& dir,
                                         std::string const& key) {
  auto cached = std::map<std::string, result>{};
  auto const d = entry_dir(dir, key);
  if (!std::filesystem::is_regular_file(d / "key.txt") ||
      read_file(d / "key.txt") != key) {
    return cached;
  }
  try {
    // Rows are views into these strings: they must outlive the loops.
    auto const status_file = read_file(d / "status.csv");
    for (auto const row : split_lines(status_file)) {
      auto const f = split_fields(row);
      if (f.size() != 6U) {
        throw std::runtime_error{"bad cached status row"};
      }
      cached.emplace(std::string{f[0]},
                     result{.id_ = std::string{f[0]},
                            .class_name_ = std::string{f[1]},
                            .status_ = parse_status(f[2]),
                            .status_row_ = fmt::format("{}\n", row),
                            .shape_rows_ = {},
                            .offset_rows_ = {}});
    }
    auto const attach = [&](std::string const& file, auto member) {
      auto const content = read_file(d / file);
      for (auto const row : split_lines(content)) {
        auto const it = cached.find(std::string{row_id(row)});
        if (it == cached.end()) {
          throw std::runtime_error{"cached row without status"};
        }
        (it->second.*member) += fmt::format("{}\n", row);
      }
    };
    attach("shapes.csv", &result::shape_rows_);
    attach("offsets.csv", &result::offset_rows_);
  } catch (std::exception const& e) {
    std::cerr << fmt::format("cache {} ignored: {}\n", d.string(), e.what());
    cached.clear();
  }
  return cached;
}

void save_cache(std::filesystem::path const& dir,
                std::string const& key,
                std::vector<result> const& results) {
  auto const d = entry_dir(dir, key);
  // Drop older entries: only directories this tool wrote (16 hex digits holding a key.txt).
  if (std::filesystem::is_directory(dir)) {
    for (auto const& e : std::filesystem::directory_iterator{dir}) {
      auto const name = e.path().filename().string();
      if (e.is_directory() && name.size() == 16U &&
          name.find_first_not_of("0123456789abcdef") == std::string::npos &&
          std::filesystem::is_regular_file(e.path() / "key.txt")) {
        std::filesystem::remove_all(e.path());
      }
    }
  }
  std::filesystem::create_directories(d);
  auto status = std::string{}, shapes = std::string{}, offsets = std::string{};
  for (auto const& r : results) {
    status += r.status_row_;
    shapes += r.shape_rows_;
    offsets += r.offset_rows_;
  }
  write_file(d / "status.csv", status);
  write_file(d / "shapes.csv", shapes);
  write_file(d / "offsets.csv", offsets);
  write_file(d / "key.txt", key);  // last: an entry without its key is never read
}

}  // namespace obehy::shapes
