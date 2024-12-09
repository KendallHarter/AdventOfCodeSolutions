#include "khparse.hpp"

#include "common.hpp"

std::pair<std::int64_t, std::int64_t> day4(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::capture<"[^\\n]+">,
         khparse::drop<"\\n">
      }
   };
   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto grid = res.value().value;
   int part1 = 0;
   for (int y = 0; y < grid.size(); ++y) {
      for (int x = 0; x < grid[y].size(); ++x) {
         // forward
         if (grid[y].substr(x, 4) == "XMAS") {
            part1 += 1;
         }
         // forward backwards
         if (grid[y].substr(x, 4) == "SAMX") {
            part1 += 1;
         }
         if (y + 3 < grid.size()) {
            // down
            if (grid[y][x] == 'X' && grid[y + 1][x] == 'M' && grid[y + 2][x] == 'A' && grid[y + 3][x] == 'S') {
               part1 += 1;
            }
            // down backwards
            if (grid[y][x] == 'S' && grid[y + 1][x] == 'A' && grid[y + 2][x] == 'M' && grid[y + 3][x] == 'X') {
               part1 += 1;
            }

            if (x + 3 < grid[y].size()) {
               // diagonal right-down
               if (grid[y][x] == 'X' && grid[y + 1][x + 1] == 'M' && grid[y + 2][x + 2] == 'A' && grid[y + 3][x + 3] == 'S') {
                  part1 += 1;
               }
               // diagonal right-down backwards
               if (grid[y][x] == 'S' && grid[y + 1][x + 1] == 'A' && grid[y + 2][x + 2] == 'M' && grid[y + 3][x + 3] == 'X') {
                  part1 += 1;
               }
            }

            if (x - 3 >= 0) {
               // diagonal left-down
               if (grid[y][x] == 'X' && grid[y + 1][x - 1] == 'M' && grid[y + 2][x - 2] == 'A' && grid[y + 3][x - 3] == 'S') {
                  part1 += 1;
               }
               // diagonal left-down backwards
               if (grid[y][x] == 'S' && grid[y + 1][x - 1] == 'A' && grid[y + 2][x - 2] == 'M' && grid[y + 3][x - 3] == 'X') {
                  part1 += 1;
               }
            }
         }
      }
   }

   int part2 = 0;
   for (int y = 1; y < grid.size() - 1; ++y) {
      for (int x = 1; x < grid[y].size() - 1; ++x) {
         if (grid[y][x] == 'A') {
            const auto upper_left = grid[y - 1][x - 1];
            const auto upper_right = grid[y - 1][x + 1];
            const auto lower_left = grid[y + 1][x - 1];
            const auto lower_right = grid[y + 1][x + 1];

            if (upper_left == 'M' && upper_right == 'S' && lower_left == 'M' && lower_right == 'S') {
               part2 += 1;
            }
            else if (upper_left == 'S' && upper_right == 'S' && lower_left == 'M' && lower_right == 'M') {
               part2 += 1;
            }
            else if (upper_left == 'M' && upper_right == 'M' && lower_left == 'S' && lower_right == 'S') {
               part2 += 1;
            }
            else if (upper_left == 'S' && upper_right == 'M' && lower_left == 'S' && lower_right == 'M') {
               part2 += 1;
            }
         }
      }
   }

   return {part1, part2};
}
