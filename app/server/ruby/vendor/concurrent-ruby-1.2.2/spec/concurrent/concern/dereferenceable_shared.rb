require 'concurrent/atomic/count_down_latch'

RSpec.shared_examples :dereferenceable do

  it 'defaults :dup_on_deref to false' do
    value = 'value'
    expect(value).not_to receive(:dup).with(any_args)

    subject = dereferenceable_subject(value)
    subject.value

    subject = dereferenceable_subject(value, dup_on_deref: false)
    subject.value

    subject = dereferenceable_subject(value, dup: false)
    subject.value
  end

  it 'calls #dup when the :dup_on_deref option is true' do
    value = 'value'

    subject = dereferenceable_subject(value, dup_on_deref: true)
    expect(subject.value.object_id).not_to eq value.object_id

    subject = dereferenceable_subject(value, dup: true)
    expect(subject.value.object_id).not_to eq value.object_id
  end

  it 'defaults :freeze_on_deref to false' do
    value = 'value'
    expect(value).not_to receive(:freeze).with(any_args)

    subject = dereferenceable_subject(value)
    subject.value

    subject = dereferenceable_subject(value, freeze_on_deref: false)
    subject.value

    subject = dereferenceable_subject(value, freeze: false)
    subject.value
  end

  it 'calls #freeze when the :freeze_on_deref option is true' do
    value = 'value'

    subject = dereferenceable_subject(value, freeze_on_deref: true)
    expect(subject.value).to be_frozen

    subject = dereferenceable_subject(value, freeze: true)
    expect(subject.value).to be_frozen
  end

  it 'defaults :copy_on_deref to nil' do
    value = 'value'

    subject = dereferenceable_subject(value)
    expect(subject.value.object_id).to eq(value.object_id)

    subject = dereferenceable_subject(value, copy_on_deref: nil)
    expect(subject.value.object_id).to eq(value.object_id)

    subject = dereferenceable_subject(value, copy: nil)
    expect(subject.value.object_id).to eq(value.object_id)
  end

  it 'calls the block when the :copy_on_deref option is passed a proc' do
    value = 'value'
    copy = proc{|val| 'copy' }

    subject = dereferenceable_subject(value, copy_on_deref: copy)
    expect(subject.value.object_id).not_to eq(value.object_id)

    subject = dereferenceable_subject(value, copy: copy)
    expect(subject.value.object_id).not_to eq(value.object_id)
  end

  it 'calls the :copy block first followed by #dup followed by #freeze' do
    value = 'value'
    copied = 'copied'
    dup = 'dup'
    frozen = 'frozen'
    copy = proc{|val| copied }

    expect(copied).to receive(:dup).at_least(:once).with(no_args).and_return(dup)
    expect(dup).to receive(:freeze).at_least(:once).with(no_args).and_return(frozen)

    subject = dereferenceable_subject(value, dup_on_deref: true, freeze_on_deref: true, copy_on_deref: copy)
    expect(subject.value).to eq frozen
  end

  it 'does not call #dup when #dup_on_deref is set and the value is nil' do
    allow_message_expectations_on_nil
    result = nil
    expect(result).not_to receive(:dup).with(any_args)
    subject = dereferenceable_subject(result, dup_on_deref: true)
    subject.value
  end

  it 'does not call #freeze when #freeze_on_deref is set and the value is nil' do
    allow_message_expectations_on_nil
    result = nil
    expect(result).not_to receive(:freeze).with(any_args)
    subject = dereferenceable_subject(result, freeze_on_deref: true)
    subject.value
  end

  it 'does not call the #copy_on_deref block when the value is nil' do
    copier = proc { 42 }
    subject = dereferenceable_subject(nil, copy_on_deref: copier)
    expect(subject.value).to be_nil
  end

  it 'supports dereference flags with observers' do

    if dereferenceable_subject(0).respond_to?(:add_observer)
      latch = Concurrent::CountDownLatch.new
      observer = Class.new do
        def initialize(latch)
          @latch = latch
        end
        def update(*args)
          @latch.count_down
        end
      end.new(latch)

      result = 'result'
      copier = proc { result }
      expect(result).to receive(:dup).at_least(:once).and_return(result)
      expect(result).to receive(:freeze).at_least(:once).and_return(result)
      expect(copier).to receive(:call).at_least(:once).and_return(result)

      subject = dereferenceable_observable(dup_on_deref: true, freeze_on_deref: true, copy_on_deref: copier)

      subject.add_observer(observer)
      execute_dereferenceable(subject)
      latch.wait(1)
    end
  end
end
