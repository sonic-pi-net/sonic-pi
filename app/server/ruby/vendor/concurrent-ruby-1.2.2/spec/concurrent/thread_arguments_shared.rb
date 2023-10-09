
RSpec.shared_examples :thread_arguments do

  it 'passes an empty array when opts is not given' do
    future = get_ivar_from_no_args
    expect(future.value).to eq []
  end

  it 'passes an empty array when opts is an empty hash' do
    future = get_ivar_from_args({})
    expect(future.value).to eq []
  end

  it 'passes an empty array when there is no :args key' do
    future = get_ivar_from_args(foo: 'bar')
    expect(future.value).to eq []
  end

  it 'passes an empty array when the :args key has a nil value' do
    future = get_ivar_from_args(args: nil)
    expect(future.value).to eq []
  end

  it 'passes a one-element array when the :args key has a non-array value' do
    future = get_ivar_from_args(args: 'foo')
    expect(future.value).to eq ['foo']
  end

  it 'passes an array when when the :args key has an array value' do
    expected = [1, 2, 3, 4]
    future = get_ivar_from_args(args: expected)
    expect(future.value).to eq expected
  end

  it 'passes the given array when the :args key has a complex array value' do
    expected = [(1..10).to_a, (20..30).to_a, (100..110).to_a]
    future = get_ivar_from_args(args: expected)
    expect(future.value).to eq expected
  end

  it 'allows the given arguments array to be dereferenced' do
    expected = [1, 2, 3, 4]
    future = get_ivar_from_args(args: expected)
    expect(future.value).to eq expected
  end
end
