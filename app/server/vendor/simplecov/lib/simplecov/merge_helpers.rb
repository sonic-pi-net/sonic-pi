module SimpleCov
  module ArrayMergeHelper
    # Merges an array of coverage results with self
    def merge_resultset(array)
      new_array = dup
      array.each_with_index do |element, i|
        pair = [element, new_array[i]]
        new_array[i] = if pair.any?(&:nil?) && pair.map(&:to_i).all?(&:zero?)
                         nil
                       else
                         element.to_i + new_array[i].to_i
                       end
      end
      new_array
    end
  end
end

module SimpleCov
  module HashMergeHelper
    # Merges the given Coverage.result hash with self
    def merge_resultset(hash)
      new_resultset = {}
      (keys + hash.keys).each do |filename|
        new_resultset[filename] = nil
      end

      new_resultset.each_key do |filename|
        result1 = self[filename]
        result2 = hash[filename]
        new_resultset[filename] =
          (result1 && result2) ? result1.extend(ArrayMergeHelper).merge_resultset(result2) : (result1 || result2).dup
      end
      new_resultset
    end
  end
end
