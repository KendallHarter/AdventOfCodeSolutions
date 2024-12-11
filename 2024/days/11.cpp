#include "khparse.hpp"

#include "common.hpp"

#include <cmath>
#include <unordered_map>

std::vector<std::int64_t> cycle1(const std::vector<std::int64_t>& in)
{
   std::vector<std::int64_t> to_ret;
   for (const auto& val : in) {
      const auto num_digits = 1 + static_cast<int>(std::floor(std::log10(val)));
      if (val == 0) {
         to_ret.push_back(1);
      }
      else if (num_digits % 2 == 0) {
         // This is lazy but it works
         const auto str = std::format("{}", val);
         std::string_view str_view = str;
         to_ret.push_back(to_int<std::int64_t>(str_view.substr(0, num_digits / 2)));
         to_ret.push_back(to_int<std::int64_t>(str_view.substr(num_digits / 2)));
      }
      else {
         to_ret.push_back(val * 2024);
      }
   }
   return to_ret;
}

std::unordered_map<std::int64_t, std::int64_t> cycle2(const std::unordered_map<std::int64_t, std::int64_t>& in)
{
   std::unordered_map<std::int64_t, std::int64_t> to_ret;
   for (const auto& [number, count] : in) {
      const auto num_digits = 1 + static_cast<int>(std::floor(std::log10(number)));
      if (number == 0) {
         to_ret[1] += count;
      }
      else if (num_digits % 2 == 0) {
         const auto str = std::format("{}", number);
         std::string_view str_view = str;

         to_ret[to_int<std::int64_t>(str_view.substr(0, num_digits / 2))] += count;
         to_ret[to_int<std::int64_t>(str_view.substr(num_digits / 2))] += count;
      }
      else {
         to_ret[number * 2024] += count;
      }
   }
   return to_ret;
}

std::pair<std::int64_t, std::int64_t> day11(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::with_skipper,
      khparse::drop<"\\s">,
      khparse::i64,
      1,
      0
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }

   const auto start = res.value().value;

   auto part1_stones = start;
   for (std::size_t i = 0; i < 25; ++i) {
      part1_stones = cycle1(part1_stones);
   }
   const auto part1 = std::ssize(part1_stones);

   std::unordered_map<std::int64_t, std::int64_t> part2_stones;
   for (const auto& val : start) {
      part2_stones[val] += 1;
   }
   for (std::size_t i = 0; i < 75; ++i) {
      part2_stones = cycle2(part2_stones);
   }

   const auto part2 = std::ranges::fold_left(
      part2_stones,
      0ll,
      [](const auto& sum, const auto& p) {
         return sum + p.second;
      }
   );

   return {part1, part2};
};
