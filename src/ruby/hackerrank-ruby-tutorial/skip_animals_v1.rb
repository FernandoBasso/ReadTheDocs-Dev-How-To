#
# tags: [enumerables, each_with_index]
#

#
# rubocop:disable Metrics/BlockLength
#

require 'rspec'

##
# Return the array without the first `skip` elements.
#
def skip_animals(animals, skip)
  skipped_with_index = []

  animals.each_with_index do |animal, idx|
    idx >= skip && skipped_with_index << "#{idx}:#{animal}"
  end

  skipped_with_index
end

describe 'skip_animals()' do
  it 'should not skip any animals with skip of 0' do
    expect(
      skip_animals(%w[penguin bear fox wolf], 0)
    ).to eq %w[0:penguin 1:bear 2:fox 3:wolf]
  end

  it 'should skip first animal with skip of 1' do
    expect(
      skip_animals(%w[penguin bear fox wolf], 1)
    ).to eq %w[1:bear 2:fox 3:wolf]
  end

  it 'should skip first animal with skip of 2' do
    expect(
      skip_animals(%w[penguin bear fox wolf], 2)
    ).to eq %w[2:fox 3:wolf]
  end

  it 'should return only last animal if skip arraay length - 1' do
    expect(
      skip_animals(%w[penguin bear fox wolf], 3)
    ).to eq ['3:wolf']
  end

  it 'should skip all animals if skip is >= array length' do
    ##
    # `skip` is exactly the array length (4).
    #
    expect(
      skip_animals(%w[penguin bear fox wolf], 4)
    ).to eq []

    ##
    # `skip` is more than the array length (5).
    #
    expect(
      skip_animals(%w[penguin bear fox wolf], 4)
    ).to eq []
  end
end
