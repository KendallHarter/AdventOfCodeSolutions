#include "khparse.hpp"

#include "common.hpp"

#include <unordered_map>
#include <unordered_set>
#include <ranges>

std::pair<std::int64_t, std::int64_t> day8(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::with_skipper,
      khparse::drop<"\\n">,
      khparse::capture<"[^\\n]+">,
      1,
      0
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }

   const auto map = res.value().value;
   const auto map_height = map.size();
   const auto map_width = map[0].size();

   const std::unordered_map<char, std::vector<std::pair<int, int>>> beacon_locs = [&]() {
      std::unordered_map<char, std::vector<std::pair<int, int>>> to_ret;
      for (const auto& [y, line] : std::views::enumerate(map)) {
         for (const auto& [x, c] : std::views::enumerate(line)) {
            if (c != '.') {
               to_ret[c].emplace_back(x, y);
            }
         }
      }
      return to_ret;
   }();

   const auto hash_func = [](const std::pair<int, int>& to_hash) {
      return (to_hash.first << 16) | to_hash.second;
   };
   std::unordered_set<std::pair<int, int>, decltype(hash_func)> antinode_locs;

   for (const auto& [c, locs] : beacon_locs) {
      for (const auto& [i, loc] : std::views::enumerate(locs)) {
         for (const auto& other_loc : locs | std::views::drop(i + 1)) {
            const auto x_diff = loc.first - other_loc.first;
            const auto y_diff = loc.second - other_loc.second;
            const auto x1 = loc.first + x_diff;
            const auto y1 = loc.second + y_diff;
            const auto x2 = other_loc.first - x_diff;
            const auto y2 = other_loc.second - y_diff;

            for (const auto& [x, y] : {std::make_pair(x1, y1), std::make_pair(x2, y2)}) {
               if (x >= 0 && x < map_width && y >= 0 && y < map_height) {
                  antinode_locs.emplace(x, y);
               }
            }
         }
      }
   }

   const std::int64_t part1 = std::ssize(antinode_locs);

   for (const auto& [c, locs] : beacon_locs) {
      for (const auto& [i, loc] : std::views::enumerate(locs)) {
         for (const auto& other_loc : locs | std::views::drop(i + 1)) {
            const auto x_diff = loc.first - other_loc.first;
            const auto y_diff = loc.second - other_loc.second;

            for (const auto& [x_off, y_off] : {std::pair{x_diff, y_diff}, std::pair{-x_diff, -y_diff}}) {
               auto x = loc.first;
               auto y = loc.second;
               while (x >= 0 && x < map_width && y >= 0 && y < map_height) {
                  antinode_locs.emplace(x, y);
                  x += x_off;
                  y += y_off;
               }
            }
         }
      }
   }

   const std::int64_t part2 = std::ssize(antinode_locs);

   return {part1, part2};
}
