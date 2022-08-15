#include <algorithm>
#include <charconv>
#include <cstdint>
#include <cstring>
#include <iterator>

// Standard output functions allocate so they can buffer
// but we don't want buffering for once
#include <unistd.h>

struct iter_sentinel {};

// I could probably make this more generic but meehhhh
struct iter {
public:
   iter() noexcept : write_buf_loc{0}, is_complete{false} { advance(); }

   void advance() noexcept {
      read(STDIN_FILENO, buffer + write_buf_loc, sizeof(buffer) - write_buf_loc);
      const auto [rest, ec] = std::from_chars(std::begin(buffer), std::end(buffer), value);
      if (ec == std::errc{}) {
         const char* start = std::begin(buffer);
         const char* end = std::end(buffer);
         auto iter = std::find_if(rest, end, [](int c) { return !std::isspace(c); });
         const auto shift_size = std::distance(start, iter);
         // std::shift_left(std::begin(buffer), std::end(buffer), shift_size);
         std::rotate(std::begin(buffer), std::begin(buffer) + shift_size, std::end(buffer));
         write_buf_loc = sizeof(buffer) - shift_size;
         // write 0 to remainder of buffer so that it won't parse garbage
         std::fill(std::begin(buffer) + write_buf_loc, std::end(buffer), '\0');
      }
      else {
         is_complete = true;
      }
   }

   constexpr std::int32_t operator*() const noexcept
   {
      return value;
   }

   void operator++() noexcept { advance(); }

   friend constexpr bool operator==(const iter& a, iter_sentinel) noexcept { return a.is_complete; }
   friend constexpr bool operator!=(const iter& a, iter_sentinel) noexcept { return !a.is_complete; }

private:
   std::uint32_t value;
   std::int8_t write_buf_loc : 7;
   bool is_complete : 1;
   // This is probably too large but eh
   char buffer[16];
};

struct range_dummy {
   iter begin() const noexcept { return {}; }
   iter_sentinel end() const noexcept { return {}; }
};

void output(int val)
{
   char buffer[16];
   const auto [end_buf, ec] = std::to_chars(std::begin(buffer), std::end(buffer), val);
   if (ec == std::errc{}) {
      write(STDOUT_FILENO, buffer, end_buf - buffer);
   }
}

void output(const char* c)
{
   write(STDOUT_FILENO, c, std::strlen(c));
}

void output(char c)
{
   write(STDOUT_FILENO, &c, 1);
}

int calc_part_two(int so_far)
{
   const int next = so_far / 3 - 2;
   if (next <= 0) { return 0; }
   else { return so_far + calc_part_two(next); }
}

int main()
{
   int part1 = 0;
   int part2 = 0;
   for (const auto& val : range_dummy{}) {
      part1 += val / 3 - 2;
      part2 += calc_part_two(val / 3 - 2);
   }
   output("Part 1: ");
   output(part1);
   output('\n');
   output("Part 2: ");
   output(part2);
   output('\n');
}
