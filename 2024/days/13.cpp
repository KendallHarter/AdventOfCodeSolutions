#include "khparse.hpp"

#include "common.hpp"

#include <boost/multiprecision/cpp_int.hpp>

#include <cmath>

#include <iostream>

std::pair<std::int64_t, std::int64_t> day13(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::drop<"Button A: X\\+">,
         khparse::i64,
         khparse::drop<", Y\\+">,
         khparse::i64,
         khparse::drop<"\\nButton B: X\\+">,
         khparse::i64,
         khparse::drop<", Y\\+">,
         khparse::i64,
         khparse::drop<"\\nPrize: X=">,
         khparse::i64,
         khparse::drop<", Y=">,
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

   const auto cranes = res.value().value;

   int part1 = 0;
   for (const auto& crane : cranes) {
      const auto& [a_x, a_y, b_x, b_y, prize_x, prize_y] = crane;

      int num_a_presses = 0;
      int num_b_presses = 0;

      const auto calc_crane_loc = [&]() -> std::pair<std::int64_t, std::int64_t> {
         return {a_x * num_a_presses + b_x * num_b_presses, a_y * num_a_presses + b_y * num_b_presses};
      };

      bool solution_found = true;
      while (calc_crane_loc() != std::pair{prize_x, prize_y}) {
         num_b_presses += 1;
         const auto [new_x, new_y] = calc_crane_loc();
         if (new_x > prize_x || new_y > prize_y) {
            num_b_presses = 0;
            num_a_presses += 1;
            const auto [newer_x, newer_y] = calc_crane_loc();
            if (newer_x > prize_x || newer_y > prize_y) {
               solution_found = false;
               break;
            }
         }
      }

      if (solution_found) {
         part1 += num_a_presses * 3 + num_b_presses;
      }
   }

   using bigint = boost::multiprecision::cpp_int;
   bigint part2 = 0;
   for (const auto& crane : cranes) {
      const auto& [a_x, a_y, b_x, b_y, raw_prize_x, raw_prize_y] = crane;
      const auto prize_x = raw_prize_x + 10000000000000;
      const auto prize_y = raw_prize_y + 10000000000000;

      // Solve a system of equations :O
      // [[a, b]; [c, d]]^-1 * [e, f] = [x, y]
      // (1 / (a * b - b * c)) * [[d, -b]; [-c, a]] * [e * f] = [x, y]
      // x = d * e / (a * d - b * c) - b * f / (a * d - b * c)
      // y = a * f / (a * d - b * c) - c * e / (a * d - b * c)
      const bigint a = a_x;
      const bigint b = b_x;
      const bigint c = a_y;
      const bigint d = b_y;
      const bigint e = prize_x;
      const bigint f = prize_y;
      const bigint divisor = a * d - b * c;

      const bigint a_presses = d * e / divisor - b * f / divisor;
      const bigint b_presses = a * f / divisor - c * e / divisor;

      if (a_presses * a_x + b_presses * b_x == prize_x && a_presses * a_y + b_presses * b_y == prize_y) {
         part2 += a_presses * 3 + b_presses;
      }
   }

   std::cout << "(Too big for i64) Part 2: " << part2 << '\n';

   return {part1, 0};
}
