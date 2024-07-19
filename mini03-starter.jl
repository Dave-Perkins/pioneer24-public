mutable struct Triple 
    x::Int 
    y::Int 
    z::Int 
    x_conflicts::Int 
    y_conflicts::Int 
    z_conflicts::Int
end

"Return the first available filename of the form input#.txt from the inputs folder."
function get_filename()
    biggest_number = 1
    while isfile(pwd() * "/inputs/" * "input" * string(biggest_number) * ".txt")
        biggest_number = biggest_number + 1
    end
    return "input" * string(biggest_number) * ".txt"
    # return "input1.txt" # use this for debugging
end

"Write a set of triples to a file, where each line is a triple (e.g. 3 6 6)"
function write_triples_to_file(filename, triples)
    open(pwd() * "/inputs/" * filename, "w") do f
        for triple in triples
            write(f, "$(triple.x) $(triple.y) $(triple.z) \n")
        end
    end
end

"Read a graph from a file, where each line is a pair of node keys (e.g. 2 4)"
function read_data(filename)
    triples = Vector{Triple}()
    io = open(pwd() * "/inputs/" * filename, "r");
    for line in eachline(io)
        x, y, z = [parse(Int, ss) for ss in split(line)]
        t = Triple(x, y, z, false, false, false)
        push!(triples, t)
    end
    return triples
end

function create_triples(filename, size_T, size_x, size_y, size_z)
    filename == "" || return read_data(filename)
    filename = get_filename() 
    triples = Vector{Triple}()
    for _ in 1:size_T 
        t = (rand(1:size_x), rand(1:size_y), rand(1:size_z))
        while t in [(tr.x, tr.y, tr.z) for tr in triples] 
            t = (rand(1:size_x), rand(1:size_y), rand(1:size_z))
        end
        push!(triples, Triple(t[1], t[2], t[3], 0, 0, 0))
    end
    write_triples_to_file(filename, triples)
    return triples 
end

function pretty_print(triples)
    for t in triples 
        println([(t.x, t.y, t.z), (t.x_conflicts, t.y_conflicts, t.z_conflicts)])
    end
end

function main(filename = "input3.txt", size_T = 32, size_x = 16, size_y = 16, size_z = 16, C = 100, P = 0.95, a = 200)
    triples = create_triples(filename, size_T, size_x, size_y, size_z)
    pretty_print(triples) 
end

main()
