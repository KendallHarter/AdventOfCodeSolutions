#include "khparse.hpp"

#include "common.hpp"

#include <unordered_map>
#include <unordered_set>
#include <ranges>
#include <numeric>

std::pair<std::int64_t, std::int64_t> day5(const std::string& input)
{
   const auto parser = khparse::seq{
      khparse::repeat{
         khparse::seq{
            khparse::i64, khparse::drop<"\\|">, khparse::i64, khparse::drop<"\\n">
         }
      },
      khparse::drop<"\\n">,
      khparse::repeat{
         khparse::seq{
            khparse::repeat{
               khparse::with_skipper,
               khparse::drop<",">,
               khparse::i64,
               1,
               0
            },
            khparse::drop<"\\n">
         }
      }
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto& [page_requirements, page_lists] = res.value().value;

   std::unordered_map<std::int64_t, std::unordered_set<std::int64_t>> page_rules;
   for (const auto& [before_page, after_page] : page_requirements) {
      page_rules[before_page].insert(after_page);
   }

   const auto is_valid = [&](const std::vector<std::int64_t>& line) {
      for (std::size_t i = 1; i < line.size(); ++i) {
         const auto current = line[i];
         for (const auto& before : line | std::views::take(i)) {
            if (page_rules[current].count(before) > 0) {
               return false;
            }
         }
      }
      return true;
   };

   int part1 = 0;
   for (const auto& line : page_lists) {
      if (is_valid(line)) {
         part1 += *std::midpoint(line.data(), line.data() + line.size());
      }
   }

   int part2 = 0;

   for (const auto& line : page_lists) {
      if (!is_valid(line)) {
         const auto fixed_line = [&]() -> std::vector<std::int64_t> {
            auto line_copy = line;
            for (std::size_t i = 0; i < line_copy.size(); ++i) {
               for (std::size_t j = i + 1; j < line_copy.size(); ++j) {
                  if (page_rules[line_copy[i]].count(line_copy[j]) == 0) {
                     std::swap(line_copy[i], line_copy[j]);
                  }
               }
            }
            return line_copy;
         }();
         part2 += *std::midpoint(fixed_line.data(), fixed_line.data() + fixed_line.size());
      }
   }

   return {part1, part2};
}
