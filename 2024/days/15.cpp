#include "khparse.hpp"

#include "common.hpp"

#include <unordered_set>

std::pair<std::int64_t, std::int64_t> day15(const std::string& input)
{
   const auto parser = khparse::seq{
      khparse::repeat{
         khparse::seq{
            khparse::capture<"[^\\n]+">,
            khparse::drop<"\\n">
         },
         1,
         0
      },
      khparse::drop<"\\n">,
      khparse::repeat{
         khparse::with_skipper,
         khparse::drop<"\\n">,
         khparse::capture<"[^\\n]+">,
         1,
         0
      }
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }

   const auto& [raw_map, raw_directions] = res.value().value;

   const auto get_offset = [](char dir) -> std::pair<int, int> {
      switch (dir) {
      case '^': return { 0, -1};
      case 'v': return { 0,  1};
      case '<': return {-1,  0};
      case '>': return { 1,  0};
      default:
         eprint("Invalid direction {}\n", dir);
         std::exit(1);
      }
   };

   const auto directions = [&]() {
      std::vector<char> dirs;
      // inefficient but easy
      for (const auto& dir_line : raw_directions) {
         for (const auto& c : dir_line) {
            dirs.push_back(c);
         }
      }
      return dirs;
   }();

   const auto part1 = [&]() {
      const auto height = std::ssize(raw_map);
      const auto width = std::ssize(raw_map[0]);
      std::vector<std::string> map(raw_map.begin(), raw_map.end());
      const auto [start_x, start_y] = [&]() {
         for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
               if (map[y][x] == '@') {
                  return std::make_pair(x, y);
               }
            }
         }
         eprint("No starting point found\n");
         std::exit(1);
      }();

      // remove the robot from the map
      map[start_y][start_x] = '.';

      auto x = start_x;
      auto y = start_y;
      for (const auto& dir : directions) {
         const auto [x_off, y_off] = get_offset(dir);

         const auto new_x = x + x_off;
         const auto new_y = y + y_off;
         if (map[new_y][new_x] == '.') {
            x = new_x;
            y = new_y;
         }
         else if (map[new_y][new_x] == 'O') {
            int len = 1;
            while (map[y + y_off * len][x + x_off * len] == 'O') {
               len += 1;
            }
            const auto final_x = x + x_off * len;
            const auto final_y = y + y_off * len;
            // Only move if there's space
            if (map[final_y][final_x] == '.') {
               for (int i = 1; i <= len; ++i) {
                  map[y + y_off * i][x + x_off * i] = 'O';
               }
               x = new_x;
               y = new_y;
               map[y][x] = '.';
            }
         }
      }

      std::int64_t part1 = 0;
      for (int y = 0; y < height; ++y) {
         for (int x = 0; x < width; ++x) {
            if (map[y][x] == 'O') {
               part1 += y * 100;
               part1 += x;
            }
         }
      }

      return part1;
   }();

   const auto part2 = [&]() {
      // there's almost certainly a better way to do this but oh well
      std::vector<std::string> map;
      for (const auto& line : raw_map) {
         map.emplace_back();
         for (const auto& c : line) {
            switch (c) {
            case '#':
               map.back().push_back('#');
               map.back().push_back('#');
               break;

            case 'O':
               map.back().push_back('[');
               map.back().push_back(']');
               break;

            case '.':
               map.back().push_back('.');
               map.back().push_back('.');
               break;

            case '@':
               map.back().push_back('@');
               map.back().push_back('.');
               break;

            default:
               eprint("Illegal character {}\n", c);
               std::exit(1);
            }
         }
      }

      const auto height = std::ssize(map);
      const auto width = std::ssize(map[0]);
      const auto [start_x, start_y] = [&]() {
         for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
               if (map[y][x] == '@') {
                  return std::make_pair(x, y);
               }
            }
         }
         eprint("No starting point found\n");
         std::exit(1);
      }();

      // erase robot
      map[start_y][start_x] = '.';

      auto x = start_x;
      auto y = start_y;
      for (const auto& dir : directions) {
         const auto [x_off, y_off] = get_offset(dir);
         const auto is_box = [&](const std::pair<int, int>& p) {
            return map[p.second][p.first] == '[' || map[p.second][p.first] == ']';
         };

         const auto new_x = x + x_off;
         const auto new_y = y + y_off;
         if (map[new_y][new_x] == '.') {
            x = new_x;
            y = new_y;
         }
         else if (map[new_y][new_x] == '[' || map[new_y][new_x] == ']') {
            // handle horizontal and vertical pushing differently
            if (x_off != 0) {
               // horizontal pushing
               auto len = 3;
               while (map[y][x + x_off * len] == map[new_y][new_x]) {
                  len += 2;
               }
               if (map[y][x + x_off * len] != '#') {
                  if (x_off == 1) {
                     for (int i = len - 2; i > 0; i -= 2) {
                        map[y][x + i + 0] = '.';
                        map[y][x + i + 1] = '[';
                        map[y][x + i + 2] = ']';
                     }
                  }
                  else if (x_off == -1) {
                     for (int i = len - 2; i > 0; i -= 2) {
                        map[y][x - i - 0] = '.';
                        map[y][x - i - 1] = ']';
                        map[y][x - i - 2] = '[';
                     }
                  }
                  x = new_x;
                  y = new_y;
               }
            }
            else {
               // vertical pushing
               const auto hasher_func = [](const auto& p) { return (p.first << 16) | p.second; };
               using hasher = decltype(hasher_func);
               const auto cur_box_off = map[new_y][new_x] == '[' ? 0 : -1;
               std::unordered_set<std::pair<int, int>, hasher> boxes_to_move{{new_x + cur_box_off, new_y}};
               std::vector<std::pair<int, int>> prev_boxes{{new_x + cur_box_off, new_y}};
               while (!prev_boxes.empty()) {
                  // for each box in the previous push, check if there is another box above/below it
                  const auto boxes = std::move(prev_boxes);
                  // Ensure that the previous boxes are cleared since the state is valid but unknown
                  prev_boxes.clear();
                  for (const auto& box : boxes) {
                     for (const auto& add_x : {0, 1}) {
                        if (is_box({box.first + add_x, box.second + y_off})) {
                           if (map[box.second + y_off][box.first + add_x] == '[') {
                              boxes_to_move.emplace(box.first + add_x, box.second + y_off);
                              prev_boxes.emplace_back(box.first + add_x, box.second + y_off);
                           }
                           else {
                              boxes_to_move.emplace(box.first + add_x - 1, box.second + y_off);
                              prev_boxes.emplace_back(box.first + add_x - 1, box.second + y_off);
                           }
                        }
                        else if (map[box.second + y_off][box.first + add_x] == '#') {
                           // we hit a wall - cancel all movement
                           boxes_to_move.clear();
                           goto movement_done;
                        }
                     }
                  }
               }
               movement_done:
               if (!boxes_to_move.empty()) {
                  // erase old boxes and then move them
                  for (const auto& [box_x, box_y] : boxes_to_move) {
                     map[box_y][box_x] = '.';
                     map[box_y][box_x + 1] = '.';
                  }
                  for (const auto& [box_x, box_y] : boxes_to_move) {
                     map[box_y + y_off][box_x] = '[';
                     map[box_y + y_off][box_x + 1] = ']';
                  }
                  x = new_x;
                  y = new_y;
               }
            }
         }
      }

      std::int64_t part2 = 0;
      for (int y = 0; y < height; ++y) {
         for (int x = 0; x < width; ++x) {
            if (map[y][x] == '[') {
               part2 += y * 100;
               part2 += x;
            }
         }
      }

      return part2;
   }();

   print("Part 2 is WRONG\n");
   return {part1, part2};
}
