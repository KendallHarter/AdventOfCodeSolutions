#include "khparse.hpp"

#include "common.hpp"

#include <ranges>

std::pair<std::int64_t, std::int64_t> day1(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::with_skipper,
         khparse::drop<"\\s">,
         khparse::i64,
         khparse::i64
      },
      1,
      0
   };
   const auto result = parser.parse(input);
   if (!result.has_value()) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto values = result.value().value;
   const auto sort_list = [&]<std::size_t I>(constant<I>) {
      auto r = values | std::views::elements<I>;
      std::vector<std::int64_t> to_ret(r.begin(), r.end());
      std::ranges::sort(to_ret);
      return to_ret;
   };
   const auto list1 = sort_list(constant<0>{});
   const auto list2 = sort_list(constant<1>{});
   const auto part1 = std::ranges::fold_left(
      std::views::zip(list1, list2),
      0ll,
      [](std::int64_t total, const std::pair<std::int64_t, std::int64_t>& vals) noexcept -> std::int64_t {
         return total + std::abs(vals.first - vals.second);
      }
   );

   std::unordered_map<std::int64_t, std::int64_t> number_counts;
   for (const auto& val : list2) {
      number_counts[val] += 1;
   }
   const auto part2 = std::ranges::fold_left(
      list1,
      0ll,
      [&](std::int64_t total, std::int64_t val) noexcept -> int {
         return total + val * number_counts[val];
      }
   );

   return {part1, part2};
}
