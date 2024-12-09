#include "khparse.hpp"

#include "common.hpp"

#include <ranges>

std::pair<std::int64_t, std::int64_t> day2(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::repeat{
            khparse::seq{
               khparse::with_skipper,
               khparse::drop<" ">,
               khparse::i64
            }
         },
         khparse::drop<"\\n">
      },
      1,
      0
   };
   const auto result = parser.parse(input);
   if (!result.has_value()) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto diff_is_safe = [](bool is_increasing, std::int64_t val1, std::int64_t val2) noexcept -> bool {
      const auto diff = static_cast<int>(std::abs(val1 - val2));
      if (diff == 0 || diff > 3) {
         return false;
      }
      if (is_increasing && val1 > val2) {
         return false;
      }
      if (!is_increasing && val1 < val2) {
         return false;
      }
      return true;
   };

   const auto line_is_safe = [&](const auto& line) noexcept {
      if (line.size() < 2 || line[0] == line[1]) {
         return false;
      }
      const bool is_increasing = line[0] < line[1];
      for (const auto& [val1, val2] : std::views::zip(line, line | std::views::drop(1))) {
         if (!diff_is_safe(is_increasing, val1, val2)) {
            return false;
         }
      }
      return true;
   };

   int part1 = 0;
   for (const auto& line : result.value().value) {
      if (line_is_safe(line)) {
         part1 += 1;
      }
   }

   int part2 = 0;
   for (const auto& line : result.value().value) {
      const bool is_safe = [&]() {
         if (line_is_safe(line)) {
            return true;
         }
         for (std::size_t i = 0; i < line.size(); ++i) {
            // No concat view :c
            // so do this instead
            auto line_copy = line;
            line_copy.erase(line_copy.begin() + i);
            if (line_is_safe(line_copy)) {
               return true;
            }
         }
         return false;
      }();
      if (is_safe) {
         part2 += 1;
      }
   }

   return {part1, part2};
}
