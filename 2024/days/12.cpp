#include "khparse.hpp"

#include "common.hpp"

#include <array>
#include <unordered_set>
#include <unordered_map>
#include <stack>
#include <ranges>

const std::array offsets = {std::pair{-1, 0}, std::pair{1, 0}, std::pair{0, -1}, std::pair{0, 1}};

std::pair<std::int64_t, std::int64_t> day12(const std::string& input)
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

   const auto hasher_func = [](const std::pair<int, int>& p) { return (p.first << 16) | p.second; };
   using hasher = decltype(hasher_func);

   const auto plots = [&]() {
      std::vector<std::pair<char, std::unordered_set<std::pair<int, int>, hasher>>> plots;
      std::unordered_set<std::pair<int, int>, hasher> seen;
      for (int start_y = 0; start_y < map_height; ++start_y) {
         for (int start_x = 0; start_x < map_width; ++start_x) {
            std::stack<std::pair<int, int>> frontier;
            if (!seen.contains({start_x, start_y})) {
               frontier.emplace(start_x, start_y);
               plots.push_back({map[start_y][start_x], {}});
               while (!frontier.empty()) {
                  const auto [x, y] = frontier.top();
                  frontier.pop();
                  seen.emplace(x, y);
                  plots.back().second.emplace(x, y);

                  for (const auto& [x_off, y_off] : offsets) {
                     const auto new_x = x + x_off;
                     const auto new_y = y + y_off;
                     if (new_x < 0 || new_x >= map_width || new_y < 0 || new_y >= map_height) {
                        continue;
                     }
                     else if (map[y][x] == map[new_y][new_x] && !seen.contains({new_x, new_y})) {
                        frontier.emplace(new_x, new_y);
                     }
                  }
               }
            }
         }
      }
      return plots;
   }();

   std::int64_t part1 = 0;
   for (const auto& [crop, locs] : plots) {
      const int area = std::ssize(locs);
      int perimeter = 0;
      for (const auto& [x, y] : locs) {
         // for each direction if there's no plot in the specified direction it means
         // that there's an edge there so add 1 if that's the case
         for (const auto& [x_off, y_off] : offsets) {
            const auto new_x = x + x_off;
            const auto new_y = y + y_off;
            if (new_x < 0 || new_x >= map_width || new_y < 0 || new_y >= map_height) {
               perimeter += 1;
            }
            else if (!locs.contains({new_x, new_y})) {
               perimeter += 1;
            }
         }
      }
      part1 += area * perimeter;
   }

   std::int64_t part2 = 0;
   for (const auto& [crop, locs] : plots) {
      const int area = std::ssize(locs);
      std::array<std::unordered_set<std::pair<int, int>, hasher>, 4> side_lists;
      for (const auto& [x, y] : locs) {
         for (const auto& [i, offset] : std::views::enumerate(offsets)) {
            const auto& [x_off, y_off] = offset;
            const auto new_x = x + x_off;
            const auto new_y = y + y_off;
            if (new_x < 0 || new_x >= map_width || new_y < 0 || new_y >= map_height) {
               side_lists[i].emplace(new_x, new_y);
            }
            else if (!locs.contains({new_x, new_y})) {
               side_lists[i].emplace(new_x, new_y);
            }
         }
      }
      int sides = 0;
      // calculate the number of sides
      // for each direction, count the number of areas that can be accessed; this will
      // equal the number of sides
      for (const auto& side_locs : side_lists) {
         std::unordered_set<std::pair<int, int>, hasher> seen_side_locs;
         for (const auto& loc : side_locs) {
            const auto& [start_x, start_y] = loc;
            std::stack<std::pair<int, int>> frontier;
            if (!seen_side_locs.contains({start_x, start_y})) {
               sides += 1;
               frontier.emplace(start_x, start_y);
               while (!frontier.empty()) {
                  const auto [x, y] = frontier.top();
                  frontier.pop();
                  seen_side_locs.emplace(x, y);

                  for (const auto& [x_off, y_off] : offsets) {
                     const auto new_x = x + x_off;
                     const auto new_y = y + y_off;
                     if (side_locs.contains({new_x, new_y}) && !seen_side_locs.contains({new_x, new_y})) {
                        frontier.emplace(new_x, new_y);
                     }
                  }
               }
            }
         }
      }
      part2 += area * sides;
   }

   return {part1, part2};
}
