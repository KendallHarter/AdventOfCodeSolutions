#include "khparse.hpp"

#include "common.hpp"

std::pair<std::int64_t, std::int64_t> day3(const std::string& input)
{
   const auto mul_parser = khparse::seq{
      khparse::drop<"mul\\(">,
      khparse::i64,
      khparse::drop<",">,
      khparse::i64,
      khparse::drop<"\\)">
   };
   const auto do_parser = khparse::drop<"do\\(\\)">;
   const auto do_not_parser = khparse::drop<"don't\\(\\)">;
   std::int64_t part1 = 0;
   for (const char* str = input.c_str(); str != input.c_str() + input.size();) {
      const auto res = mul_parser.parse(std::string_view{str, input.c_str() + input.size()});
      if (res) {
         const auto [val1, val2] = res.value().value;
         part1 += val1 * val2;
         str = res.value().rest;
      }
      else {
         ++str;
      }
   }

   std::int64_t part2 = 0;
   bool enabled = true;
   for (const char* str = input.c_str(); str != input.c_str() + input.size();) {
      const auto sub_input = std::string_view{str, input.c_str() + input.size()};
      if (enabled) {
         if (const auto res = do_not_parser.parse(sub_input)) {
            enabled = false;
            str = res.value().rest;
         }
         else if (const auto res = mul_parser.parse(sub_input)) {
            const auto [val1, val2] = res.value().value;
            part2 += val1 * val2;
            str = res.value().rest;
         }
         else {
            ++str;
         }
      }
      else {
         if (const auto res = do_parser.parse(sub_input)) {
            enabled = true;
            str = res.value().rest;
         }
         else {
            ++str;
         }
      }
   }

   return {part1, part2};
}
