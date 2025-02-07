"""
This is the skeleton (that is, the starter code) for Pioneer Summer 2024 Mini-project 1.
"""

using Graphs, GraphRecipes, Plots   # for plotting graphs
using StatsBase                     # for sample
using Combinatorics                 # for combinations
using Colors                        # to access RGB colors

mutable struct Node  
    key::Int
    degree::Int
    color::Int 
    neighbors::Vector{Int}
end

"Creates a random graph (as a .txt) as well as a SimpleGraph graph."
function setup(num_nodes, edge_multiplier, filename)
    if filename == ""
        next_available_filename = get_filename()
        println("Using this input file: $next_available_filename")
        make_random_graph(next_available_filename, num_nodes, edge_multiplier)
    else 
        next_available_filename = filename 
    end
    edges = read_data(next_available_filename)
    edge_list = Edge.(edges)
    return SimpleGraph(edge_list), edges
end

"Write a new random graph to a file, where each line is a pair of node keys (e.g. 2 4)."
function make_random_graph(filename, num_nodes, edge_multiplier)
    open(pwd() * "/input_graphs/" * filename, "w") do f
        nodes = [x for x in 1:num_nodes]
        combos = collect(Combinatorics.combinations(nodes, 2))
        edges = collect(sample(combos, trunc(Int, edge_multiplier * num_nodes); replace = false))
        for edge in edges
            write(f, "$(edge[1]) $(edge[2]) \n")
        end
    end
end

"Read a graph from a file, where each line is a pair of node keys (e.g. 2 4)"
function read_data(filename)
    edges = Tuple{Int, Int}[]
    io = open(pwd() * "/input_graphs/" * filename, "r");
    for line in eachline(io)
        x, y = [parse(Int, ss) for ss in split(line)]
        t = Tuple([x, y])
        push!(edges, t)
    end
    return edges
end

"Display the given graph."
function viewgraph(graph, nodes) 
    nodecolors = [node.color for node in nodes] 
    max_color, _ = findmax(nodecolors)
    colors = [RGB(min.(0.99, 0.5 + rand()/2), min.(0.99, 0.5 + rand()/2), min.(0.99, 0.5 + rand()/2)) for _ in 1:max_color] 
    use_colors = [nodecolors[i] == 0 ? RGB(0.8) : colors[nodecolors[i]] for i in 1:nv(graph)] 
    p = graphplot(graph,
        names = 1:nv(graph),
        fontsize = 14,
        nodecolor = use_colors,
        nodelabeldist = 5, 
        nodelabelangleoffset = π/4,
        markershape = :circle,
        markersize = 0.15,
        markerstrokewidth = 2,
        markerstrokecolor = :gray,
        edgecolor = :gray,
        linewidth = 2,
        curves = true
    )
    display(p)
end

"Return the first available filename of the form g#.txt from the input_graphs folder."
function get_filename()
    biggest_number = 1
    while isfile(pwd() * "/input_graphs/" * string('g') * string(biggest_number) * ".txt")
        biggest_number = biggest_number + 1
    end
    return string('g') * string(biggest_number) * ".txt"
    # return "g1.txt" # use this for debugging
end

"Determine the number of happy edges in the given graph (represented by its nodes)."
function count_happy_edges(nodes)
    nodecolors = [n.color for n in nodes]
    num_happies = 0
    for node in nodes
        for nbr in node.neighbors 
            num_happies += (node.color != nodecolors[nbr])
        end
    end
    return num_happies ÷ 2
end

"Creates an array of Node structs as a way of representing the given graph."
function create_nodes_vector(graph)
    nodes = Vector{Node}()
    for key in 1:nv(graph)
        deg = length(all_neighbors(graph, key))
        new_node = Node(key, deg, 0, all_neighbors(graph, key))
        push!(nodes, new_node) 
    end
    return nodes
end

"""
Algorithm 1 Iterated Greedy (IG)
Input: A simple connected graph G = (V,E), with all vertices uncolored, and list of distinct colors
Output: A proper vertex coloring
ω ← highest-degree first ordering of V 
for vertex u in ω do
    assign to u the smallest color that is not already used by one of u’s neighbors 
end for
"""

"High level code for the Iterated Greedy algorithm."
function greedy(graph)
    nodes = create_nodes_vector(graph)
    sorted_nodes = sort(nodes, by = x -> x.degree, rev = true)
    for node in sorted_nodes 
        neighbor_colors = [nodes[neighbor].color for neighbor in node.neighbors]
        max_value, _ = findmax(neighbor_colors)
        for c in 1:(max_value + 1)
            if !(c in neighbor_colors)
                node.color = c
                break 
            end
        end
    end
    return nodes 
end

"Report results for Iterated Greedy algorithm."
function report_greedy(graph, edges, nodes)
    println("--- Iterated Greedy algorithm ---")
    print("number of edges in graph: ")
    println(length(edges))
    length(edges) < 63 && viewgraph(graph, nodes) 
    num_happies_greedy = count_happy_edges(nodes)
    print("number of happy edges: ")
    println(num_happies_greedy)
    print("number of colors used: ")
    println(length(Set([node.color for node in nodes])))
end

"Main control for this program."
function main(num_nodes = 7, edge_multiplier = 1, filename = "g6.txt")
    graph, edges = setup(num_nodes, edge_multiplier, filename)
    nodes = greedy(graph)
    report_greedy(graph, edges, nodes)
end

main()
