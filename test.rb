class Board
  def initialize
    @board = []
    81.times { @board.push %w{P p N n B b R r L l G g S s K k}[rand(30)] || 'X' }
  end
  def to_s
    @board.join.unpack("A9"*9).join("\n").gsub(/(.)/,'\1 ')
  end
end

if __FILE__ == $0
  puts Board.new.to_s
end
