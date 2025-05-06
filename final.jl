using Random
using StatsBase 
using IterTools
using Combinatorics
using ProgressMeter
using DataStructures
using Statistics

function print_statistics(data)
    println("Mean: ", mean(data))
    println("Variance: ", var(data))
    println("Standard Deviation: ", std(data))
    println("Minimum: ", minimum(data))
    println("Maximum: ", maximum(data))
    println("--------------------------\n")
    
    
end

mutable struct Triple 
    i::Int 
    j::Int 
    k::Int 
end

Base.:(==)(a::Triple, b::Triple) = (a.i == b.i && a.j == b.j && a.k == b.k)

function create_constraints(n, m)
    constraints = []
    @showprogress for i in 1:m
        temp = sample(1:n, 3; replace=false)
        triple = Triple(temp[1], temp[2], temp[3])
        while(triple in constraints || Triple(triple.k, triple.j, triple.i) in constraints)
            temp = sample(1:n, 3; replace=false)
            triple = Triple(temp[1], temp[2], temp[3])
        end
        push!(constraints, triple)
    end
    return constraints
end

function print_constraints(constraints)

    println()
    for triple in constraints
        println("($(triple.i), $(triple.j), $(triple.k))")
    end
    println()

end

function write_file(constraints, n, m, ans, ansCombi, name)
    open(pwd() * "/inputs/" * "$name", "w") do file
        write(file, "$n\n$m\n$ans\n")
        line = join(ansCombi, " ")
        write(file, line * "\n")
        for triple in constraints
            write(file, "$(triple.i) $(triple.j) $(triple.k)\n")
        end
    end
end

function read_file(name)
    constraints = Vector{Triple}()
    n = -1
    m = -1
    ans = -1
    ansCombi = []
    open(pwd() * "/inputs/" * "$name", "r") do file
        n = parse(Int, readline(file))
        m = parse(Int, readline(file))
        ans = parse(Int, readline(file))
        ansCombi = split(readline(file))

        for line in eachline(file)
            values = split(line)
            i = parse(Int, values[1])
            j = parse(Int, values[2])
            k = parse(Int, values[3])
            push!(constraints, Triple(i, j, k))
        end
    end
    return n, m, ans, ansCombi, constraints
end

function get_filename()
    biggest_number = 1
    while isfile(pwd() * "/inputs/" * "input" * string(biggest_number) * ".txt")
        biggest_number = biggest_number + 1
    end
    return "input" * string(biggest_number) * ".txt"
    # return "input1.txt" # use this for debugging
end


function iterate_file(n, m)
    constraints = create_constraints(n, m)
    println("constructed")
    ans = 0
    ansCombi = [0]
    #ansCombi, ans = brute_force(constraints, n)
    name = get_filename()
    write_file(constraints, n, m, ans, ansCombi, name)
    println("Created File $name")
end

#O(n! * m * n)
function brute_force(constraints, n)

    maxCombination = collect(1:n)
    maxConstraints = constraints_satisfied(constraints, maxCombination)
    @showprogress for trial in permutations(1:n)
        current = constraints_satisfied(constraints, trial)
        "println()
        println(current)
        @show trial
        println()
        readline()"
        if (current > maxConstraints)
            maxConstraints = current
            maxCombination = trial
        end
    end

    return maxCombination, maxConstraints
end

function constraints_satisfied(constraints, combination)
    satisfied = 0
    for constraint in constraints
        i_pos = findfirst(x -> x == constraint.i, combination)
        j_pos = findfirst(x -> x == constraint.j, combination)
        k_pos = findfirst(x -> x == constraint.k, combination)
    
        if (i_pos < j_pos < k_pos) || (k_pos < j_pos < i_pos)
            satisfied += 1
        end
    end
    return satisfied
end


function main()
    n = 10
    m = 10
    constraints = create_constraints(n, m)
    print_constraints(constraints)
    combination, satisfied = brute_force(constraints, n)
    println("Result: $satisfied out of $m")
    @show combination

end

function main1()
    constraints = [Triple(2,1,3), Triple(3,4,5), Triple(1,4,5), Triple(2,4,1), Triple(5,2,3)]
    combi = [3, 1, 4, 2, 5]
    println(constraints_satisfied(constraints, combi))
end

function makeFiles(n = 500, m = 2000, numTimes = 1)
    if (n * (n-1) * (n-2)/2 < m)
        println("Failed, inputted m excedes maximum amount of possible constrictions")
        return
    end

    for i in 1:numTimes
        iterate_file(n, m)
    end
end
#makeFiles()

function solve_betweenness(constraints, n)
    M = copy(constraints)
    V = Vector{Vector{Int}}()

    while !isempty(M)
        j_counts = counter([c.j for c in M])
        c = argmax(c -> j_counts[c.j], M)

        function find_index(x)
            for (i, v) in enumerate(V)
                if x in v
                    return i
                end
            end
            return nothing
        end
        #c = M[1]

        ind_i, ind_j, ind_k = find_index(c.i), find_index(c.j), find_index(c.k)
        #@show c
        #println("$ind_i, $ind_j, $ind_k")

        if isempty(V)
            push!(V, [c.i], [c.j], [c.k])
            continue
        end
        if ind_i === ind_j === ind_k === nothing
            insert!(V, ceil(Int, length(V)/2), [c.j])
            pushfirst!(V, [c.i])
            push!(V, [c.k])
            continue
        end

        if ind_j !== nothing
            if ind_i === ind_k === nothing
                if ind_j != length(V)
                    push!(V[length(V)], c.k)
                else
                    push!(V, [c.k])
                end

                if ind_j != 1
                    push!(V[1], c.i)
                else
                    pushfirst!(V, [c.k])
                end
            elseif ind_i !== nothing && ind_k === nothing
                if ind_j > ind_i
                    push!(V[length(V)], c.k)
                elseif ind_j < ind_i
                    pushfirst!(V[1], c.k)
                else
                    new_v = [c.j]
                    deleteat!(V[ind_j], findfirst(x -> x == c.j, V[ind_j]))
                    if ind_j > length(V) ÷ 2
                        insert!(V, ind_j, new_v)
                    else
                        insert!(V, ind_j + 1, new_v)
                    end
                    push!(V[length(V)], c.k)
                end
            elseif ind_k !== nothing && ind_i === nothing
                if ind_j > ind_k
                    push!(V[length(V)], c.i)
                elseif ind_j < ind_k
                    pushfirst!(V[1], c.i)
                else
                    new_v = [c.j]
                    deleteat!(V[ind_j], findfirst(x -> x == c.j, V[ind_j]))
                    if ind_j > length(V) ÷ 2
                        insert!(V, ind_j, new_v)
                    else
                        insert!(V, ind_j + 1, new_v)
                    end
                    push!(V[length(V)], c.i)
                end
            end
        else  # c.j isn't in V
            if ind_i !== nothing && ind_k !== nothing
                if ind_i == ind_k
                    if ind_i < length(V)/2
                        
                        deleteat!(V[ind_k], findfirst(x-> x == c.k, V[ind_k]))
                        insert!(V, ind_i + 1, [c.j])
                        push!(V[ind_i+2], c.k)
                    else
                        deleteat!(V[ind_k], findfirst(x-> x == c.k, V[ind_k]))
                        insert!(V, ind_i - 1, [c.j])
                        push!(V[ind_i-2], c.k)
                    end
                else #SPREAD
                    mid = (ind_i + ind_k) ÷ 2
                    ind = min(ind_i, ind_k)
                    if mid == ind #if k and i are next to each other
                        insert!(V, mid+1, [c.j])
                    else
                        push!(V[mid], c.j) 
                    end
                end
            elseif ind_k !== nothing
                if (ind_k <= length(V))
                    push!(V[1], c.i)
                    ind_i = 1
                else
                    push!(V[length(V)], c.i)
                    ind_i = length(V)
                end
                mid = (ind_i + ind_k) ÷ 2
                ind = min(ind_i, ind_k)
                if mid == ind
                    insert!(V, mid+1, [c.j])
                else
                    push!(V[mid], c.j) 
                end
            elseif ind_i !== nothing
                if (ind_i <= length(V))
                    push!(V[1], c.k)
                    ind_k = 1
                else
                    push!(V[length(V)], c.k)
                    ind_k = length(V)
                end
                mid = (ind_i + ind_k) ÷ 2
                ind = min(ind_i, ind_k)
                if mid == ind
                    insert!(V, mid+1, [c.j])
                else
                    push!(V[mid], c.j) 
                end
            end
        end

        "
        if 
        
        "

        deleteat!(M, findfirst(x -> x == c, M))
        #@show V
    end

    # Flatten V and add unused integers
    result = vcat(V...)
    for i in 1:n
        if i ∉ result
            push!(result, i)
        end
    end

    return result
end

#called only when i and j are placed
#index of i must be before or the same as k ----------------------------------------
function spreadJ(ind_i, ind_j, ind_k, V, c, threshold)
    ind_j = find_index(c.j, V)
    if (ind_j == nothing)
        if (ind_i == ind_k)
            insert!(V, ind_i+1, Set([c.j]))
            insert!(V, ind_i+2, Set([c.k]))
            delete!(V[ind_k], c.k)
        elseif (ind_k - ind_i == 1)
            insert!(V, ind_i+1, Set([c.j]))
        else
            if (rand()>0.5)
                push!(V[ind_i + rand(1:(ind_k-ind_i-1))], c.j)
            else
                mid = (ind_i + ind_k) ÷ 2
                push!(V[mid], c.j)
            end
        end
    else
        #@show ind_j
        if (ind_j > ind_k || ind_j < ind_i) #nothing can be done, j is already placed outside of valid bounds
            return 1
        elseif (ind_i == ind_j == ind_k)
            delete!(V[ind_j], c.i)
            delete!(V[ind_j], c.k)
            insert!(V, ind_j, Set([c.i]))
            insert!(V, ind_j+2, Set([c.k]))
        elseif (ind_j == ind_k)
            delete!(V[ind_j], c.j)
            insert!(V, ind_j, Set([c.j]))
        elseif (ind_j == ind_i)
            delete!(V[ind_j], c.j)
            insert!(V, ind_j+1, Set([c.j]))
        end
    end
    return 0
end

function find_index(x, V)
    for (i, v) in enumerate(V)
        if x in v
            return i
        end
    end
    return nothing
end

function find_middle(V)
    all_elements = collect(Iterators.flatten(V))
    middle_index = div(length(all_elements) + 1, 2)
    middle_element = all_elements[middle_index]


    for i in 1:length(V)
        if middle_element in V[i]
            return i
        end
    end
    
    return nothing
end

function simAnneal(c, choice)
    if (choice == 1)
        a = 15
        return exp(-a/c) # normal
    elseif (choice == 2)
        a = 200
        return 7*exp(a/(c-100)) # reverse
    elseif (choice == 3)
        a = 1000
        return exp(-((c-50)^2)/a) # bell curve
    elseif (choice == 4)
        return 1.0 # debug
    elseif (choice == 5)
        a = 1000
        return 1-exp(-((c-50)^2)/a)
    elseif (choice == 7)
        return 1/(1-exp(-0.2(c-70)))
    end
    return 0.0
end

function simAnnealAssist(x)
    #return 100.0*x
    return 100.0*exp(-0.04x)
end

function collapse(ind_i, ind_j, ind_k, V, c, threshold)
    if (ind_i != nothing && ind_k != nothing)
    elseif (ind_i == ind_k == nothing)
        push!(V[1], c.i)
        ind_i = 1
        push!(V[length(V)], c.k)
        ind_k = length(V)
    else # if one of them is nothing but not the other
        if (ind_i == nothing) # k is always the one that needs to be placed
            ind_i , ind_k = ind_k, ind_i
            c.i , c.k = c.k , c.i 
        end

        if (ind_j != nothing && ind_j != ind_i)
            if (ind_j > ind_i)
                push!(V[length(V)], c.k)
                ind_k = length(V)
            else
                push!(V[1], c.k)
                ind_k = 1
            end
        else
            middle_set_index = find_middle(V)
            if (ind_i > middle_set_index)
                push!(V[1], c.k)
                ind_k = 1
            else
                push!(V[length(V)], c.k)
                ind_k = length(V)
            end
        end
    end
    if (ind_i > ind_k)
        ind_i , ind_k = ind_k, ind_i
        c.i , c.k = c.k , c.i 
    end
    return spreadJ(ind_i, ind_j, ind_k, V, c, threshold)
end

function findMostConstricted(constraints)
    j_count = Dict{Int, Int}()
    for triple in constraints
        j_count[triple.j] = get(j_count, triple.j, 0) + 1
    end
    max_j_value = findmax(j_count)[2]
    common_j_subvector = [triple for triple in constraints if triple.j == max_j_value]
    return common_j_subvector
end

function wfcv2(constraints, n, choice)
    M = deepcopy(constraints)
    V = Vector{Set{Int}}()
    H = []
    push!(V, Set([]))

    total = length(M)
    i = 1
    while !isempty(M)
        temperature = 100*((i)/total)
        #threshold = simAnneal(simAnnealAssist(temperature), choice)
        threshold = simAnneal(temperature, choice)
        mostConstricted = findMostConstricted(M)
        c = nothing
        if (rand() < threshold)
            c = rand(M)
        else
            c = rand(mostConstricted)
        end

        
        cCopy = Triple(c.i, c.j, c.k)
        ind_i, ind_j, ind_k = find_index(c.i, V), find_index(c.j, V), find_index(c.k, V)
        
        if (rand() < 0.5)
            ind_i , ind_k = ind_k, ind_i
            c.i , c.k = c.k , c.i 
        end
        failed = collapse(ind_i, ind_j, ind_k, V, c, threshold)
        if (failed == 0)
            push!(H, cCopy) 
        end
        deleteat!(M, findfirst(x -> x == c, M))
        i = i+1
    end
    result = collect(Iterators.flatten(V))
    for i in 1:n
        if i ∉ result
            push!(result, i)
        end
    end

    h = []
    for triple in H
        push!(h, triple.i)
        push!(h, triple.j)
        push!(h, triple.k)
    end


    return result, h
end

function main3(filename = "input243.txt")
    n, m, satisfied, combination, constraints = read_file(filename)
    #print_constraints(constraints)
    
    println("Solving using brute force method:")
    combination, satisfied = brute_force(constraints, n)
    println("Brute force result: $satisfied out of $m")
    #@show combination
    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 = wfcv2(constraints, n, 6)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")
    
    
    N = 100
    #println("\nSolving using betweenness method using SimAnneal:")
    MAXChoice = 1
    MAX = -1.0
    for choice in 1:5
        Max = -Inf
        Min = Inf
        betweenness_satisfied = 0.0
        data = []
        
        for i in 1:N
            betweenness_result = wfcv2(constraints, n, choice)
            satisfied = constraints_satisfied(constraints, betweenness_result)
            push!(data, satisfied)
            #@show betweenness_result
            
        end
        #println("$choice Betweenness result: $betweenness_satisfied out of $m, MAX: $Max, MIN: $Min")
        #@show choice
        print_statistics(data)
        meanData = mean(data)
    end
    
end

function main4(filename = "input81.txt")
    n, m, satisfied, combination, constraints = read_file(filename)
    #print_constraints(constraints)
    
    println("Solving using brute force method:")
    combination, satisfied = brute_force(constraints, n)
    println("Brute force result: $satisfied out of $m")
    #@show combination
    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 = wfcv2(constraints, n, 6)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")
    
    
    N = 100
    #println("\nSolving using betweenness method using SimAnneal:")
    MAXChoice = 1
    MAX = -1.0
    for choice in 1:5
        Max = -Inf
        Min = Inf
        betweenness_satisfied = 0.0
        data = []
        
        for i in 1:N
            betweenness_result = wfcv2(constraints, n, choice)
            satisfied = constraints_satisfied(constraints, betweenness_result)
            push!(data, satisfied)
            
        end
        #println("$choice Betweenness result: $betweenness_satisfied out of $m, MAX: $Max, MIN: $Min")
        #@show choice
        print_statistics(data)
        meanData = mean(data)
    end
    
end



function main2(filename = "input22.txt")
    n, m, satisfied, combination, constraints = read_file(filename)
    #print_constraints(constraints)
    #=
    println("Solving using brute force method:")
    combination, satisfied = brute_force(constraints, n)
    println("Brute force result: $satisfied out of $m")
    #@show combination
    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 = wfcv2(constraints, n, 5)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")
    =#
    
    N = 5
    #println("\nSolving using betweenness method using SimAnneal:")
    MAXChoice = 1
    MAX = -1.0
    for choice in 1:5
        Max = -Inf
        Min = Inf
        betweenness_satisfied = 0.0
        data = []
        
        for i in 1:N
            betweenness_result = wfcv2(constraints, n, choice)
            satisfied = constraints_satisfied(constraints, betweenness_result)
            push!(data, satisfied)
            
        end
        #println("$choice Betweenness result: $betweenness_satisfied out of $m, MAX: $Max, MIN: $Min")
        #@show choice
        #print_statistics(data)
        meanData = mean(data)
        if (meanData > MAX)
            MAXChoice = choice
            MAX = meanData
        end
    end
    return MAXChoice
    
end

function supermain()
    score = [0, 0, 0, 0, 0]
    @showprogress for i in 84:184
        winner = main2("input$i.txt")
        score[winner] = score[winner] + 1
    end
    @show score
end

function crossover_permutation(parent1::Vector{Int}, parent2::Vector{Int})
    n = length(parent1)
    @assert n == length(parent2) "Parents must have the same length"
    @assert Set(parent1) == Set(1:n) "Parent1 must be a permutation of 1:n"
    @assert Set(parent2) == Set(1:n) "Parent2 must be a permutation of 1:n"

    # Choose a random crossover point
    crossover_point = rand(1:n-1)

    # Initialize child with the first part of parent1
    child = parent1[1:crossover_point]

    # Fill the rest of the child with elements from parent2
    for i in parent2
        if !(i in child)
            push!(child, i)
        end
    end

    return child
end

function solver(filename = "input260.txt", bruteforce = false)
    n, m, satisfied, combination, constraints = read_file(filename)

    if (bruteforce)
        println("Solving using brute force method:")
        combination, satisfied = brute_force(constraints, n)
        println("Brute force result: $satisfied out of $m")
        @show combination
    end

    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 , _ = wfcv2(constraints, n, 6)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")

    P = []
    key = [4]
    N = 250÷length(key)
    @showprogress for i in key
        for l in 1:N
            betweenness_result, H = wfcv2(constraints, n, i)
            satisfied = constraints_satisfied(constraints, betweenness_result)
            push!(P, (betweenness_result, satisfied, H))
        end
    end

    data = [x[2] for x in P]
    @show mean(data)
    @show maximum(data)

    data = [x[2] for x in P]
    @show mean(data)
    @show maximum(data)

    #totalMax = argmax(t -> t[2], P)
    #@show totalMax[2]

    #@show constraints_satisfied(constraints, totalMax[1])

end

function solver2(filename = "mbp-50-400.txt", bruteforce = false)
    n, m, satisfied, combination, constraints = read_file(filename)

    if (bruteforce)
        println("Solving using brute force method:")
        combination, satisfied = brute_force(constraints, n)
        println("Brute force result: $satisfied out of $m")
        @show combination
    end

    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 , _ = wfcv2(constraints, n, 6)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")

    P = []
    key = [7]
    N = 250÷length(key)
    #@showprogress for i in key
    #    for l in 1:N
    #        betweenness_result, H = wfcv2(constraints, n, i)
    #        satisfied = constraints_satisfied(constraints, betweenness_result)
    #        push!(P, (betweenness_result, satisfied, H))
    #    end
    #end

    for i in 1:250
        v = randperm(n)
        push!(P, (v, constraints_satisfied(constraints, v), []))
    end

    data = [x[2] for x in P]
    @show mean(data)
    @show maximum(data)

    len = length(P)
    @showprogress for i in 1:(Int)(n*3)
        for index in 1:length(P)
            p = P[index]
            
            current, fitness, H = p
           # freq = countmap(H)
           # ranked_numbers = collect(freq)
           # ranked_numbers = [x[1] for x in ranked_numbers]
           # sorted = sort(collect(freq), by=x -> -x[2])
           # ranked_numbers = [x[1] for x in sorted]
           # weights = [2^i for i in 1:length(ranked_numbers)]
           # sum_weights = sum(weights)
           # weights = weights / sum_weights
            
           # ranked_numbers = ranked_numbers[Int(ceil(length(ranked_numbers)*0.25)):end]
           # ranked_numbers = ranked_numbers[1:(length(ranked_numbers)÷2)]
            v = []
            for j in 1:300
                currentP = deepcopy(current)
                #x = sample(ranked_numbers, Weights(weights))
                #y = sample(ranked_numbers, Weights(weights))
                
                x = sample(1:n)
                y = sample(1:n)
                currentP[x], currentP[y] = currentP[y], currentP[x]
                

                newFitness = constraints_satisfied(constraints, currentP)
                if (newFitness > satisfied)
                    push!(v, (currentP, newFitness, H))
                end
            end
            if (!isempty(v))
                maxInd = argmax(t -> t[2], v)
                P[index] = maxInd
                #@show index
            end

            
        end
        
        second_values = [t[2] for t in P]
        
        if (length(second_values) > 0)
            threshold = mean(second_values)
        end
        #println("Before $(length(P))")
        P = filter(t -> (t[2] >= threshold || rand()>0.1), P)
        #println("After $(length(P))")
        if (rand() < 0.2 && length(P) > 1)
            top_2 = partialsort(P, 1:2, by = t -> t[2], rev = true)
            childOrdering = crossover_permutation(top_2[1][1], top_2[2][1])
            child = childOrdering , constraints_satisfied(constraints, childOrdering), []
            push!(P, child)
        end
    end
    #second_values = [t[2] for t in P]
    #@show second_values
    data = [x[2] for x in P]
    @show mean(data)
    @show maximum(data)

    #totalMax = argmax(t -> t[2], P)
    #@show totalMax[2]

    #@show constraints_satisfied(constraints, totalMax[1])
    return maximum(data)
end

function solver3(filename = "mbp-50-400.txt", bruteforce = false)
    n, m, satisfied, combination, constraints = read_file(filename)

    if (bruteforce)
        println("Solving using brute force method:")
        combination, satisfied = brute_force(constraints, n)
        println("Brute force result: $satisfied out of $m")
        @show combination
    end

    println("\nSolving using betweenness method NO meta:")
    betweenness_result1 , _ = wfcv2(constraints, n, 6)
    betweenness_satisfied1 = constraints_satisfied(constraints, betweenness_result1)
    println("Betweenness result: $betweenness_satisfied1 out of $m")

    P = []
    key = [7]
    N = (n*2)÷length(key)
    @showprogress for i in key
        for l in 1:N
            betweenness_result, H = wfcv2(constraints, n, i)
            satisfied = constraints_satisfied(constraints, betweenness_result)
            push!(P, (betweenness_result, satisfied, H))
        end
    end

    data = [x[2] for x in P]
    @show mean(data)
    @show maximum(data)

    len = length(P)
    for i in 1:(Int)(n*2)
        for index in 1:length(P)
            p = P[index]
            
            current, fitness, H = p
            v = []
            for j in 1:n*3
                currentP = deepcopy(current)
                
                x = sample(1:n)
                y = sample(1:n)
                currentP[x], currentP[y] = currentP[y], currentP[x]
                

                newFitness = constraints_satisfied(constraints, currentP)
                if (newFitness > satisfied)
                    push!(v, (currentP, newFitness, H))
                end
            end
            if (!isempty(v))
                maxInd = argmax(t -> t[2], v)
                P[index] = maxInd
            end

            
        end
        
        second_values = [t[2] for t in P]
        
        if (length(second_values) > 0)
            threshold = mean(second_values)
        end
        #println("Before $(length(P))")
        P = filter(t -> (t[2] >= threshold || rand()>0.1), P)
        #println("After $(length(P))")
        if (rand() < 0.2 && length(P) > 1)
            top_2 = partialsort(P, 1:2, by = t -> t[2], rev = true)
            childOrdering = crossover_permutation(top_2[1][1], top_2[2][1])
            child = childOrdering , constraints_satisfied(constraints, childOrdering), []
            push!(P, child)
        end
    end
    #second_values = [t[2] for t in P]
    #@show second_values
    data = [x[2] for x in P]
    @show mean(data)
     

    #totalMax = argmax(t -> t[2], P)
    #@show totalMax[2]

    #@show constraints_satisfied(constraints, totalMax[1])
    
    return maximum(data)
end

function mainactual(name)
    totalTime = []
    for i in 1:20
        time = @elapsed result = solver2(name, false)
        @show result
        push!(totalTime, (result, time))
    end

    # Find the tuple with the maximum result
    max_result_tuple = argmax(x -> x[1], totalTime)
    
    # Find the index of the first instance of the maximum result tuple
    max_index = findfirst(x -> x == max_result_tuple, totalTime)
    
    # Sum up all the times of the elements before and including the max result
    sum_times = sum(x[2] for x in totalTime[1:max_index])
    
    # Calculate the mean of the results
    mean_result = mean(x[1] for x in totalTime)

    # Print the results
    println("Sum of times up to max result: ", sum_times)
    println("Mean of results: ", mean_result)
    println("Max of results: ", max_result_tuple[1])

    @show (sum(x[2] for x in totalTime) / 20)
end
#mainactual()
# Run the main2 function
#main3()
#supermain()
#main1()
#makeFiles()


