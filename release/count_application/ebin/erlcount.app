{application, erlcount,
  [{vsn, "1.0.0"},
    {modules, [erlcount, erlcount_sup, erlcount_lib,
      erlcount_dispatch, erlcount_counter]},
    {description, "Run regular expressions on Erlang source files"},
    {applications, [ppool]},
    {registered, [stdlib, kernel, erlcount]},
    {mod, {erlcount, []}},
    {env,
      [{directory, "."},
        {regex, ["if\\s.+->", "case\\s.+\\sof"]},
        {max_files, 10}]}
  ]}.