module Orego
  Location = Struct.new(:start_line, :start_column, :end_line, :end_column){
    def between(other)
      Location.new(start_line, start_column, other.end_line, other.end_column)
    end
  }
end
