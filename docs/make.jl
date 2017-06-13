try
    Pkg.installed("SymEngine")
catch
    Pkg.add("SymEngine")
end

using Documenter, Dolang

makedocs(
    modules=[Dolang],
    authors="Spencer Lyon, Pablo Winant, and contributors",
    clean=false,
    format=:html,
    sitename="Dolang.jl",
    doctest=true,
    pages=[
        "Guide" => [
            "index.md",
            "symbolic.md",
            "compiler.md",
            "examples.md",
        ],
        "Developer Documentation" => [
            "dev/compiler.md"
        ]
    ],
)

deploydocs(
    repo   = "github.com/EconForge/Dolang.jl.git",
    julia  = "0.6",
    target = "build",
    deps   = nothing,
    make   = nothing,
)
