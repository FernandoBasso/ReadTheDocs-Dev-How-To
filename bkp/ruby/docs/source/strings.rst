
Check that a string/url ends with ``/teacher/``::

Note that "/teacher/" has nine chars.

    url = 'https://domain.io/sections/teacher/'

    # v1, but does not check whether it is at the end...
    url.include?('/teacher')

    # v2, yeah, but we have to count that '/teacher/' has nine chars.
    url[-9 .. -1]

    # v3, using a regex
    url.match /\/teacher\//
    # or
    url.match %{/teacher/}

    # v4, using '/teacher/.length' and make it negative
    # and use a range.
    url.match['/teacher/.length * -1 .. -1]
