require 'rspec'
require_relative 'process_text_v1'


describe 'process_text()' do
  it 'should work with empty array of lines' do
    expect(process_text([])).to eq ''
  end

  it 'should work with array with a few lines' do
    expect(
      process_text(["Hi, \n", " Are you having fun?    "])
    ).to eq 'Hi, Are you having fun?'
  end
end
