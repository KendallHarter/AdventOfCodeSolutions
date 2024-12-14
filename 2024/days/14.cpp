#include "khparse.hpp"

#include "common.hpp"

struct robot {
   int x;
   int y;
   int x_vel;
   int y_vel;

   void move(int width, int height) noexcept
   {
      x += x_vel;
      y += y_vel;
      if (x < 0) {
         x = width + x;
      }
      else if (x >= width) {
         x = x - width;
      }
      if (y < 0) {
         y = height + y;
      }
      else if (y >= height) {
         y = y - height;
      }
   }
};

std::pair<std::int64_t, std::int64_t> day14(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::drop<"p=">,
         khparse::i64,
         khparse::drop<",">,
         khparse::i64,
         khparse::drop<" v=">,
         khparse::i64,
         khparse::drop<",">,
         khparse::i64,
         khparse::drop<"\\n*">
      },
      1,
      0
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }

   constexpr auto width = 101;
   constexpr auto height = 103;

   const auto start_robots = [&]() {
      std::vector<robot> to_ret;
      for (const auto& [x, y, x_vel, y_vel] : res.value().value) {
         to_ret.emplace_back(x, y, x_vel, y_vel);
      }
      return to_ret;
   }();

   auto robots_part1 = start_robots;
   for (int i = 0; i < 100; ++i) {
      for (auto& robot : robots_part1) {
         robot.move(width, height);
      }
   }

   std::array<std::int64_t, 4> quadrants{{0, 0, 0, 0}};
   const auto test_width = width / 2;
   const auto test_height = height / 2;
   for (const auto& robot : robots_part1) {
      if (robot.x < test_width && robot.y < test_height) {
         quadrants[0] += 1;
      }
      else if (robot.x > test_width && robot.y < test_height) {
         quadrants[1] += 1;
      }
      else if (robot.x < test_width && robot.y > test_height) {
         quadrants[2] += 1;
      }
      else if (robot.x > test_width && robot.y > test_height) {
         quadrants[3] += 1;
      }
   }

   const auto part1 = std::ranges::fold_left(quadrants, 1, std::multiplies<>{});

   return {part1, 0};
}
