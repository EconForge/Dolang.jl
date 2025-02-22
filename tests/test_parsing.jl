a = 9

using Dolang

eq = Dolang.parse_equation("fb(a[t];b[t])")


# eq = Dolang.parse_equation("a[t]-b[t]")

Dolang.convert(Expr,eq)


# Dolang.parse_equation("∀t, t>9");


# import YAML

# doc = YAML.load_file("tests/tests.yaml")

# summary = []

# for ex in doc["expressions"]
#     result = try
#         Meta.parse(ex)
#         "OK"
#     catch
#         "Failed"
#     end
#     push!(summary, (ex, result))
#     println(ex, " : ", result)
# end

# for ex in doc["symbols"]
#     result = try
#         parse(ex)
#         "OK"
#     catch
#         "Failed"
#     end
#     push!(summary, (ex, result))
#     println(ex, " : ", result)
# end

# println(summary)
