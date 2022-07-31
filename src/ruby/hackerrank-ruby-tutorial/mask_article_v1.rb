#
# tags: [string, gsub, regex, inject]
#

require 'rspec'

##
# Add HTML `<strike>` tag around `s`.
#
# Empty strings do not get surrounded with the tag lest we would get
# `<strike></strike>`.
#
def strike(s)
  return s if s.empty?

  "<strike>#{s}</strike>"
end

##
# Strikes all strings in `strs_to_mask` found in `str`.
#
# NOTE: We use `String#dup` so we don't modify the original string
# parameter.
#
def mask_article(str, strs_to_mask)
  strs_to_mask.inject(str.dup) do |acc_str, str_to_mask|
    acc_str.gsub(str_to_mask, strike(str_to_mask))
  end
end

describe 'strike()' do
  it 'should not strike empty strings' do
    expect(strike('')).to eq ''
  end

  it 'should strike non-empty strings' do
    expect(strike('crap')).to eq '<strike>crap</strike>'
    expect(
      strike('crappy shit')
    ).to eq '<strike>crappy shit</strike>'
  end
end

describe 'mask_article()' do
  it 'should mask one matching string' do
    expect(
      mask_article('What a crap!', %w[crap])
    ).to eq 'What a <strike>crap</strike>!'
  end

  it 'should mask multiple matching strings' do
    expect(
      mask_article('What a shitty dumb crap!', %w[crap shitty])
    ).to eq 'What a <strike>shitty</strike> dumb <strike>crap</strike>!'
  end
end
