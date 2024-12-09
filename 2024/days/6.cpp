#include "khparse.hpp"

#include "common.hpp"

enum struct direction : std::int8_t {
   up,
   down,
   left,
   right
};

std::pair<int, int> dir_to_offset(direction d) {
   switch (d) {
      case direction::up: return {0, -1};
      case direction::down: return {0, 1};
      case direction::left: return {-1, 0};
      case direction::right: return {1, 0};
   }
   return {0, 0};
}

direction rotate_right(direction d) {
   switch (d) {
      case direction::up: return direction::right;
      case direction::down: return direction::left;
      case direction::left: return direction::up;
      case direction::right: return direction::down;
   }
   return direction::up;
}

struct dir_set {
   bool has_dir(direction d) const noexcept
   {
      return storage_ & (1 << static_cast<int>(d));
   }

   void set_dir(direction d) noexcept
   {
      storage_ |= (1 << static_cast<int>(d));
   }

   std::uint8_t storage_ = 0x00;
};

std::pair<std::int64_t, std::int64_t> day6(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::with_skipper,
      khparse::drop<"\\n">,
      khparse::capture<"[^\\n]+">
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto raw_map = res.value().value;

   const auto map_height = raw_map.size();
   const auto map_width = raw_map[0].size();

   std::vector<std::string> map_part1(raw_map.begin(), raw_map.end());

   const std::pair<int, int> start_point = [&]() {
      for (std::size_t y = 0; y < map_part1.size(); ++y) {
         for (std::size_t x = 0; x < map_part1[y].size(); ++x) {
            if (map_part1[y][x] == '^') {
               return std::make_pair(x, y);
            }
         }
      }
      eprint("Could not find starting point\n");
      std::exit(1);
   }();

   int x = start_point.first;
   int y = start_point.second;
   auto dir = direction::up;

   map_part1[y][x] = 'x';

   while (true) {
      const auto off = dir_to_offset(dir);
      const auto new_x = x + off.first;
      const auto new_y = y + off.second;
      if (!(new_x >= 0 && new_x < map_width && new_y >= 0 && new_y < map_height)) {
         break;
      }
      if (map_part1[new_y][new_x] == '#') {
         dir = rotate_right(dir);
      }
      else {
         map_part1[new_y][new_x] = 'x';
         x = new_x;
         y = new_y;
      }
   }

   const int part1 = std::ranges::fold_left(
      map_part1,
      0,
      [](int total, const auto& list) {
         return total + std::ranges::fold_left(
            list,
            0,
            [](int total, const auto& c) {
               return total + (c == 'x');
            }
         );
      }
   );

   int part2 = 0;
   for (int obj_y = 0; obj_y < map_height; ++obj_y) {
      for (int obj_x = 0; obj_x < map_width; ++obj_x) {
         if (raw_map[obj_y][obj_x] == '#' || start_point == std::make_pair(obj_x, obj_y)) {
            continue;
         }
         std::vector<std::vector<dir_set>> dir_log(map_height, std::vector<dir_set>(map_width));

         dir_log[start_point.second][start_point.first].set_dir(direction::up);

         int x = start_point.first;
         int y = start_point.second;
         auto dir = direction::up;

         while (true) {
            const auto off = dir_to_offset(dir);
            const auto new_x = x + off.first;
            const auto new_y = y + off.second;
            if (!(new_x >= 0 && new_x < map_width && new_y >= 0 && new_y < map_height)) {
               break;
            }

            if (dir_log[new_y][new_x].has_dir(dir)) {
               part2 += 1;
               break;
            }

            if (raw_map[new_y][new_x] == '#' || (new_x == obj_x && new_y == obj_y)) {
               dir = rotate_right(dir);
            }
            else {
               dir_log[new_y][new_x].set_dir(dir);
               x = new_x;
               y = new_y;
            }
         }
      }
   }

   return {part1, part2};
}
