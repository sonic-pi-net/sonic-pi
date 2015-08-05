shared_examples :struct do

  context 'definition' do

    it 'registers the class when given a class name' do
      class_name = 'ValidClassName'
      clazz = described_class.new(class_name)
      expect{ described_class.const_get(class_name) }.to_not raise_error
      expect(clazz).to be_a Class
      expect(clazz.ancestors).to include described_class
    end

    it 'creates an anonymous class when given at least one member' do
      clazz = described_class.new(:foo)
      expect{ described_class.const_get(clazz.to_s) }.to raise_error(NameError)
      expect(clazz).to be_a Class
      expect(clazz.ancestors).to include described_class
    end

    it 'raises an exception when given an invalid class name' do
      expect{ described_class.new('lowercase') }.to raise_error(NameError)
      expect{ described_class.new('_') }.to raise_error(NameError)
      expect{ described_class.new('1') }.to raise_error(NameError)
    end

    it 'defines a getter for each member' do
      members = [:Foo, :bar, 'baz']
      structs = [
        described_class.new(*members).new,
        described_class.new('ClassForCheckingGetterDefinition', *members).new
      ]

      structs.each do |struct|
        members.each do |member|
          expect(struct).to respond_to member
          method = struct.method(member)
          expect(method.arity).to eq 0
        end
      end
    end

    it 'raises an exception when given no members' do
      expect{ described_class.new() }.to raise_error(ArgumentError)
    end

    it 'raise an exception when given an invalid member' do
      expect{ described_class.new('ClassForCheckingValidFieldNames1', 1) }.to raise_error(TypeError)
    end

    it 'evalues a given block against the new class' do
      clazz1 = described_class.new('ClassForCheckingBlockProcessing', :foo, :bar) do
        def baz(foo, bar) foo + bar; end
      end
      clazz2 = described_class.new(:foo, :bar) do
        def baz(foo, bar) foo + bar; end
      end

      [clazz1, clazz2].each do |clazz|
        struct = clazz.new
        expect(struct).to respond_to :baz
        expect(struct.method(:baz).arity).to eq 2
        expect(struct.baz(40, 2)).to eq 42
      end
    end
  end

  context 'construction' do

    let!(:members){ [:Foo, :bar, 'baz'] }
    let!(:values){ [42, '42', :fortytwo] }
    let!(:classes) do
      [
        described_class.new(*members),
        described_class.new('StructConstructionTester', *members)
      ]
    end

    it 'sets all absent members to nil' do
      classes.each do |clazz|
        struct = clazz.new
        members.each do |member|
          expect(struct.send(member)).to be_nil
        end
      end
    end

    it 'sets all given members in order' do
      classes.each do |clazz|
        struct = clazz.new(*values)
        members.each_with_index do |member, index|
          expect(struct.send(member)).to eq values[index]
        end
      end
    end

    it 'raises an exception when extra members are given' do
      classes.each do |clazz|
        extra_values = values << 'forty two'
        expect{ clazz.new(*extra_values) }.to raise_error(ArgumentError)
      end
    end
  end

  context 'properties' do

    let!(:anon_struct_members) { [:name, :address, :zip] }
    let(:anon_struct) { described_class.new(*anon_struct_members) }

    let!(:named_struct_members) { [:left, :right] }
    let(:named_struct) do
      described_class.new("Test#{described_class}Properties".gsub(/::/, ''),
                          *named_struct_members)
    end

    context '#length' do

      it 'returns the number of struct members' do
        expect(anon_struct.new.length).to eq anon_struct_members.length
        expect(named_struct.new.length).to eq named_struct_members.length
      end
    end

    context '#members' do

      it 'returns the struct members as an array of symbols' do
        expect(anon_struct.new.members).to eq anon_struct_members
        expect(named_struct.new.members).to eq named_struct_members
      end

      it 'returns a different object than the array passed at definition' do
        expect(anon_struct.new.members.object_id).to_not eq anon_struct_members.object_id
        expect(named_struct.new.members.object_id).to_not eq named_struct_members.object_id
      end
    end

    context '#size' do

      it 'returns the number of struct members' do
        expect(anon_struct.new.size).to eq anon_struct_members.size
        expect(named_struct.new.size).to eq named_struct_members.size
      end
    end

    context '#values' do

      it 'returns the values of the struct as an array in order' do
        expect(anon_struct.new().values).to eq [nil, nil, nil]
        expect(named_struct.new().values).to eq [nil, nil]

        expect(anon_struct.new(:foo, :bar, :baz).values).to eq [:foo, :bar, :baz]
        expect(named_struct.new(:yes, :no).values).to eq [:yes, :no]
      end
    end

    context '#values_at' do

      let(:anon_struct) do
        described_class.new(:zero, :one, :two, :three, :four, :five, :six, :seven, :eight, :nine).
          new(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      end

      let!(:named_struct) do
        described_class.new("Test#{described_class}ValuesAtAccessor".gsub(/::/, ''),
                            :zero, :one, :two, :three, :four, :five, :six, :seven, :eight, :nine).
                            new(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      end

      it 'returns the value at the given offset' do
        expect(anon_struct.values_at(3)).to eq [3]
        expect(named_struct.values_at(7)).to eq [7]
      end

      it 'returns the values at multiple given offsets' do
        expect(anon_struct.values_at(4, 1, 7)).to eq [4, 1, 7]
        expect(named_struct.values_at(2, 4, 6)).to eq [2, 4, 6]
      end

      it 'returns values at offsets in a given range' do
        expect(anon_struct.values_at(4..7)).to eq [4, 5, 6, 7]
        expect(named_struct.values_at(1..3)).to eq [1, 2, 3]
      end

      it 'returns values for multiple ranges' do
        expect(anon_struct.values_at(1..3, 4..7)).to eq [1, 2, 3, 4, 5, 6, 7]
        expect(named_struct.values_at(1..3, 4..7)).to eq [1, 2, 3, 4, 5, 6, 7]
      end

      it 'returns values for ranges and offsets' do
        expect(anon_struct.values_at(1, 2, 3, 4..7)).to eq [1, 2, 3, 4, 5, 6, 7]
        expect(named_struct.values_at(1, 2, 3, 4..7)).to eq [1, 2, 3, 4, 5, 6, 7]
      end
    end
  end

  context 'accessors' do

    let!(:anon_struct_members) { [:name, :address, :zip] }
    let(:anon_struct) { described_class.new(*anon_struct_members) }

    let!(:named_struct_members) { [:left, :right] }
    let(:named_struct) do
      described_class.new("Test#{described_class}Properties".gsub(/::/, ''),
                          *named_struct_members)
    end

    let(:anon_instance){ anon_struct.new('Douglass Adams', 'Earth', 42) }
    let(:named_instance){ named_struct.new('up', 'down') }

    context '#[member]' do

      it 'retrieves the value when given a valid symbol member' do
        expect(anon_instance[:address]).to eq 'Earth'
        expect(named_instance[:right]).to eq 'down'
      end

      it 'retrieves the value when given a valid string member' do
        expect(anon_instance['address']).to eq 'Earth'
        expect(named_instance['right']).to eq 'down'
      end

      it 'raises an exception when given a non-existent symbol member' do
        expect{anon_instance[:foo]}.to raise_error(NameError)
        expect{named_instance[:bar]}.to raise_error(NameError)
      end

      it 'raises an exception when given a non-existent string member' do
        expect{anon_instance['foo']}.to raise_error(NameError)
        expect{named_instance['bar']}.to raise_error(NameError)
      end
    end

    context '#[index]' do

      it 'retrieves the value when given a valid index' do
        expect(anon_instance[1]).to eq 'Earth'
        expect(named_instance[1]).to eq 'down'
      end

      it 'raises an exception when given an out-of-bound index' do
        expect{anon_instance[100]}.to raise_error(IndexError)
        expect{named_instance[100]}.to raise_error(IndexError)
      end
    end
  end

  context 'comparison' do

    let(:customer) { described_class.new(:name, :address, :zip) }
    let(:employer) { described_class.new(:name, :address, :zip) }

    let!(:joe)   { customer.new('Joe Smith', '123 Maple, Anytown NC', 12345) }
    let!(:joejr) { customer.new('Joe Smith', '123 Maple, Anytown NC', 12345) }
    let!(:jane)  { customer.new('Jane Doe', '456 Elm, Anytown NC', 12345) }
    let!(:janejr){ employer.new('Jane Doe', '456 Elm, Anytown NC', 12345) }

    context '#==' do

      it 'returns true if other has same struct subclass and equal values' do
        expect(joe == joejr).to be true
      end

      it 'returns false if other has different struct subclass' do
        expect(jane == janejr).to be false
      end

      it 'returns false if other has different values' do
        expect(jane == joe).to be false
      end
    end

    context '#!=' do

      it 'returns false if other has same struct subclass and equal values' do
        expect(joe != joejr).to be false
      end

      it 'returns true if other has different struct subclass' do
        expect(jane != janejr).to be true
      end

      it 'returns true if other has different values' do
        expect(jane != joe).to be true
      end
    end
  end

  context 'enumeration' do

    let(:members) { [:name, :address, :zip] }
    let(:values) { ['Joe Smith', '123 Maple, Anytown NC', 12345] }

    let(:customer) { described_class.new(*members) }
    let!(:joe) { customer.new(*values) }

    context '#each' do

      it 'yields the value of each struct member in order' do
        index = 0
        joe.each do |value|
          expect(joe[index]).to eq value
          index += 1
        end
        expect(index).to eq 3
      end

      it 'returns an enumerator when no block is given' do
        expect(joe.each).to be_a Enumerator
      end
    end

    context '#each_pair' do

      it 'yields the name and value of each struct member in order' do
        index = 0
        joe.each_pair do |name, value|
          expect(joe.members[index]).to eq name
          expect(joe[index]).to eq value
          index += 1
        end
        expect(index).to eq 3
      end

      it 'returns an enumerator when no block is given' do
        expect(joe.each_pair).to be_a Enumerator
      end
    end

    context '#select' do

      it 'yields each value' do
        index = 0
        joe.select do |value|
          expect(joe[index]).to eq value
          index += 1
        end
        expect(index).to eq 3
      end

      it 'returns an Array with the values from for which the block returns true' do
        result = joe.select{|value| value.is_a?(String) }
        expect(result).to eq ['Joe Smith', '123 Maple, Anytown NC']
      end

      it 'returns an enumerator when no block is given' do
        expect(joe.select).to be_a Enumerator
      end
    end
  end

  context 'conversion' do

    let!(:anon_struct_members) { [:name, :address, :zip] }
    let(:anon_struct) { described_class.new(*anon_struct_members) }

    let!(:named_struct_members) { [:left, :right] }
    let(:named_struct) do
      described_class.new("Test#{described_class}Properties".gsub(/::/, ''),
                          *named_struct_members)
    end

    context '#to_s' do

      it 'includes the name of the class when registered' do
        expect(named_struct.new.to_s).to match(/#{named_struct}/)
      end

      it 'includes the names of all members' do
        string = anon_struct.new.to_s
        anon_struct_members.each do |member|
          expect(string).to match(/#{member}/)
        end

        string = named_struct.new.to_s
        named_struct_members.each do |member|
          expect(string).to match(/#{member}/)
        end
      end

      it 'includes all values' do
        values = [:foo, 'bar', 42]
        string = anon_struct.new(*values).to_s
        values.each do |value|
          expect(string).to match(/#{value}/)
        end

        values = ['bar', 42]
        string = named_struct.new(*values).to_s
        values.each do |value|
          expect(string).to match(/#{value}/)
        end
      end

      it 'returns the same string as #inspect' do
        values = [:foo, 'bar', 42]
        struct = anon_struct.new(*values)
        expect(struct.to_s).to eq struct.inspect

        values = ['bar', 42]
        struct = named_struct.new(*values)
        expect(struct.to_s).to eq struct.inspect
      end
    end

    context '#to_a' do
      it 'returns the to_a for this struct as an array' do
        expect(anon_struct.new().to_a).to eq [nil, nil, nil]
        expect(named_struct.new().to_a).to eq [nil, nil]

        expect(anon_struct.new(:foo, :bar, :baz).to_a).to eq [:foo, :bar, :baz]
        expect(named_struct.new(:yes, :no).to_a).to eq [:yes, :no]
      end
    end

    context '#to_h' do

      it 'returns a Hash containing the names and values in order' do
        expected = {name: nil, address: nil, zip: nil}
        expect(anon_struct.new().to_h).to eq expected

        expected = {left: nil, right: nil}
        expect(named_struct.new().to_h).to eq expected

        expected = {name: :foo, address: :bar, zip: :baz}
        expect(anon_struct.new(:foo, :bar, :baz).to_h).to eq expected

        expected = {left: :yes, right: :no}
        expect(named_struct.new(:yes, :no).to_h).to eq expected
      end
    end
  end
end

shared_examples :mergeable_struct do

  let(:this){ described_class.new(:foo, :bar, :baz).new('foo', nil, nil)}
  let(:other){ {baz: 42} }

  context '#merge' do
    it 'updates all members with the new values from a given hash' do
      expect(this.merge(other).baz).to eq 42
    end

    it 'calls the given block for each key in `other`' do
      actual = 0
      this = described_class.new(:foo, :bar, :baz).new('foo', :bar, 42)
      this.merge(bar: :yes, baz: :no){|member, thisval, otherval| actual += 1 }
      expect(actual).to eq 2
    end

    it 'retains the value for all members not without values in the given hash' do
      expect(this.merge(other).foo).to eq 'foo'
    end

    it 'raises an exception when given a hash with members not in the struct' do
      expect{this.merge(bogus: true)}.to raise_exception(ArgumentError)
    end

    it 'returns a new object' do
      expect(this.merge(other).object_id).to_not eq this.object_id
      expect(this.merge(other).object_id).to_not eq other.object_id
    end
  end
end
