
list = [[1], [1], [1], [1]]

size = 1
(1..20).each do |sz_idx|
  size = sz_idx * 2 + 1
  4.times do |side|
    n = 0
    newrow = size.times.map do |i|
      if i == 0 
        if side == 0
          puts "first side, i = 0, skipping"
          next n = 0 
        else
          puts "i = 0, duping #{list[-1][-1]}"
          next n = list[-1][-1]
        end
      end

      mid_max = [i-2, 0].max
      mids = list[-4][mid_max..i]
      mids_sum = mids.inject(:+)
      puts "inner: #{list[-4].join(",")}"

      puts "adding mids: (#{mid_max}..#{i+1}): #{mids} = #{mids_sum}"
      n += mids_sum

      if i == 1 && side > 0
        n += list[-1][-2] || 0
        puts "i = 1, adding #{list[-1][-2]||0} to #{n}"
      end

      if i >= size - 2 && side == 3
        puts "last two of last side, adding #{list[-3][1]}"
        n += list[-3][1]
        list[-3][0] = n
      end

      puts "final: #{n}"
      puts
      if n > 347991
        puts "FOUND IT! #{n}"
        return
      end
      n
    end
    list << newrow
    puts newrow.join(",")
    puts
  end

  
end 
