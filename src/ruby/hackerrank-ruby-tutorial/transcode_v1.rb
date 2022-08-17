# tags: [encoding, UTF-8, ISO-8859-1]

require 'rspec'

##
# Returns the string `s` in UTF-8 encoding.
#
def transcode(s)
  s.encode('UTF-8', 'ISO-8859-1')
end

describe 'transcode()' do
  it 'should transcode from ISO-8851-1 to UTF-8' do
    expect(transcode('').encoding.name).to eq 'UTF-8'

    str = 'coração'.encode('ISO-8859-1')
    expect(transcode(str).encoding.name).to eq 'UTF-8'
  end
end
