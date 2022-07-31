#
# tags: [map, collect, rot13]
#

require 'rspec'

##
# Rotate characters 13 positions to the right.
#
# TIP: In vim, `:help rot13` or `:help g?`.
#
def rot13(s)
  s.tr('A-Za-z', 'N-ZA-Mn-za-m')
end

def decrypt_msgs(msgs)
  msgs.collect { |msg| rot13(msg) }
end

describe 'rot13()' do
  it 'should rot13 simple strings' do
    expect(rot13('abc')).to eq 'nop'
  end

  it 'should rot13 strings which wrap arond the alphabet' do
    expect(rot13('yza')).to eq 'lmn'
  end

  it 'should rot13 upper and lower case stringis' do
    expect(rot13('aBc')).to eq 'nOp'
  end
end

describe 'decrypt_msgs()' do
  it 'should return empty array with empty msgs' do
    expect(decrypt_msgs([])).to eq []
  end

  it 'should return decrypted 1-element array message' do
    expect(decrypt_msgs(['aBc'])).to eq ['nOp']
  end

  it 'should return decrypted multiple-element array messages' do
    expect(
      decrypt_msgs(
        [
          'aBc',
          'yZa',
          'Why did the chicken cross the road?'
        ]
      )
    ).to eq [
      'nOp',
      'lMn',
      'Jul qvq gur puvpxra pebff gur ebnq?'
    ]
  end
end
