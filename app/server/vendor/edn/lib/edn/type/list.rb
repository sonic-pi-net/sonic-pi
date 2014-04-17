module EDN
  module Type
    include EDN::CoreExt::AllowsMetadata

    class List < ::Array
      def self.new(*values)
        self.[](*values)
      end

      def to_edn
        '(' + self.map(&:to_edn).join(" ") + ')'
      end
    end
  end
end
