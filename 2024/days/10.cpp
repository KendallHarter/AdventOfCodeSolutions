#include "khparse.hpp"

#include "common.hpp"

#include <stack>
#include <unordered_map>
#include <unordered_set>

std::pair<std::int64_t, std::int64_t> day10(const std::string& input)
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

   const auto base_search_stack = [&]() {
      std::stack<std::pair<std::pair<int, int>, std::pair<int, int>>> to_ret;
      for (int y = 0; y < map_height; ++y) {
         for (int x = 0; x < map_width; ++x) {
            if (map[y][x] == '0') {
               to_ret.push({{x, y}, {x, y}});
            }
         }
      }
      return to_ret;
   }();

   const auto hasher = [](const std::pair<int, int>& p) { return (p.first << 16) | p.second; };
   std::unordered_map<std::pair<int, int>, std::unordered_set<std::pair<int, int>, decltype(hasher)>, decltype(hasher)> seen_peaks;
   auto search_stack_part1 = base_search_stack;

   int part2 = 0;
   while (!search_stack_part1.empty()) {
      const auto [start_loc, cur_loc] = search_stack_part1.top();
      const auto [x, y] = cur_loc;
      search_stack_part1.pop();
      const auto val = map[y][x];

      if (val == '9') {
         seen_peaks[start_loc].emplace(x, y);
         part2 += 1;
      }
      else {
         const auto offsets = {std::pair{-1, 0}, std::pair{1, 0}, std::pair{0, -1}, std::pair{0, 1}};
         for (const auto& [x_off, y_off] : offsets) {
            const auto new_x = x + x_off;
            const auto new_y = y + y_off;
            if (new_x >= 0 && new_x < map_width && new_y >= 0 && new_y < map_height) {
               const auto new_val = map[new_y][new_x];
               if (new_val == val + 1) {
                  search_stack_part1.push({start_loc, {new_x, new_y}});
               }
            }
         }
      }
   }

   const auto part1 = std::ranges::fold_left(seen_peaks, 0, [](const auto& sum, const auto& peaks) { return sum + std::ssize(peaks.second); });

   return {part1, part2};
}
