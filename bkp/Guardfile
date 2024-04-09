# vim: set filetype=ruby:

require 'asciidoctor'
# require 'libnotify'

notification :notifysend, time: 15000
# notification :libnotify, timeout: 5, transient: true, append: true, urgency: :normal

guard :shell do
  watch(/.*.adoc$/) do |m|
    Asciidoctor.convert_file(
      m[0],
      :safe => :unsafe,
      :to_file => "#{m[0]}.html",
      header_footer: true,
      :attributes => {
        'favicon' => 'https://fernandobasso.dev/cmdline.png',
        # linkcss works for asciidoctor.css and pygments-default.css
        # but not for the matjax stuff.
        'linkcss!' => '',
        'toc' => 'left',
        'webfonts@' => '',
        'icons@' => 'font',
        'sectnums' => '',
        'sectlinks' => '',
        'source-highlighter' => 'pygments',
        'pygments-css' => 'class',
        'docinfo' => 'shared',
        'docinfodir' => "#{Dir.pwd}"
      }
    )
    n "#{m[0]}.html compiled successfuly!'", 'Guard Asciidoctor', :info
  end
  # notifysend "--icon='dialog-information' 'Adoc compiled successfull' -t 10"
end
