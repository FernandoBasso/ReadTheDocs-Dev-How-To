# Neovim Telescope Tips

Use `live_grep` builtin and specify extra parameters.
First take a look at `:help telescope.builtin.live_grep()`.
We'll see it takes options like `serch_dirs` and `type_filter`, among others.

```text
:Telescope live_grep type_filter=ts search_dirs=src/endpoints
```

