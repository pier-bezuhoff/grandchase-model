using Random
using Distributions
using DataStructures
using Combinatorics
using Base.Iterators
using Printf
import Base: *, -, >, show, convert, isless, iterate

@enum Star::UInt8 ☆=0x1 ☆☆ ☆☆☆ ☆☆☆☆ ☆☆☆☆☆ ☆☆☆☆☆☆
*(n::Int, s::Star)::Star =
    Star(n * UInt8(s))
-(s0::Star, s::Star)::Int8 =
    Int8(s0) - Int8(s)
inc(star::Star, d::UInt8)::Star =
    Star(UInt8(star) + d)
inc(star::Star)::Star =
    Star(UInt8(star) + 0x1)

struct Card
    star::Star
end
struct CardSymbol end
const MC = CardSymbol()
*(i::Int, ::CardSymbol)::Card =
    Card(Star(i))
isless(c0::Card, c::Card)::Bool =
    isless(c0.star, c.star)
show(io::IO, card::Card) =
    print(io, Int(card.star), "MC")
const all_cards = Card.(Star.(0x1:0x6))
const game_price_MC1 = 5 # in honor shop
const game_price_MC2 = 15
const game_price_MC3 = 45

const UpgradeLvl = UInt8 # 0..12

@inline function star_cap(star::Star)::UpgradeLvl
    if star == ☆☆☆ UpgradeLvl(3)
    elseif star == ☆☆☆☆ UpgradeLvl(6)
    elseif star == ☆☆☆☆☆ UpgradeLvl(9)
    elseif star == ☆☆☆☆☆☆ UpgradeLvl(12)
    else error("impossible hero star")
    end
end

@enum HeroRank::UInt8 H A S SR # 'H' means "any"
struct Hero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLvl
    function Hero(rank::HeroRank, star::Star, upgrade::UpgradeLvl)
        UInt8(star) >= 0x3 || error("hero star should be >= 3")
        upgrade <= star_cap(star) || error("hero upgrade capped by star")
        rank == H ||
        rank == A ||
        rank == S && UInt8(star) >= 0x4 ||
        rank == SR && UInt8(star) >= 0x5 || error("hero star capped by rank")
        new(rank, star, upgrade)
    end
    function Hero(star::Star, upgrade::UpgradeLvl)::Hero
        return Hero(H, star, upgrade)
    end
end
show(io::IO, hero::Hero) =
    print(io, hero.rank, " ", hero.star, " ", Int(hero.upgrade))
function *(n::Int, rank::HeroRank)::Hero
    ds = digits(n)
    n_digits = length(ds)
    n_digits >= 2 || error("should have >= 2 digits: first — n stars, then — upgrade lvl")
    return Hero(rank, Star(last(ds)), UpgradeLvl(n % 10 ^ (n_digits - 1)))
end
iterate(hero::Hero, s=1) =
    if s == 1 (hero.rank, 2)
    elseif s == 2 (hero.star, 3)
    elseif s == 3 (hero.upgrade, 4)
    else nothing
    end
@inline function inc_to(hero::Hero, target::Hero)::Hero
    (rank, star, lvl) = hero
    (target_rank, target_star, target_lvl) = target
    rank != target_rank && error("different hero ranks")
    star > target_star && error("target star < hero star")
    lvl > target_lvl && error("target upgrade < hero upgrade")
    if lvl == target_lvl
        return target
    end
    cap::UpgradeLvl = star_cap(star)
    if lvl == cap || lvl + 1 == cap && star < target_star
        star = inc(star)
    end
    lvl += one(UpgradeLvl)
    return Hero(hero.rank, star, lvl)
end

struct HeroUpgrade
    hero::Hero
    target::Hero
    function HeroUpgrade(hero::Hero, target::Hero)
        target.upgrade >= hero.upgrade || error("target hero upgrade < current hero upgrade")
        hero.upgrade <= star_cap(hero.star) && target.upgrade <= star_cap(target.star) || error("upgrade > star cap")
        hero.rank == target.rank || error("inconsistent hero rank")
        new(hero, target)
    end
end
show(io::IO, up::HeroUpgrade) =
    print(io, up.hero, " ↦ ", up.target)
>(hero::Hero, target::Hero)::HeroUpgrade =
    HeroUpgrade(hero, target)
iterate(up::HeroUpgrade, s=1) =
    if s == 1 (up.hero, 2)
    elseif s == 2 (up.target, 3)
    else nothing
    end

# const Prob = Rational{Int} # try/cmp performance
const Prob = Float32
const ProbBonus = Prob

@inline function _rel_prob(d::Int8)::Prob
    if d < 0x1 Prob(1)
    elseif d == 0x1 Prob(1//2)
    elseif d == 0x2 Prob(1//4)
    elseif d == 0x3 Prob(1//10)
    elseif d == 0x4 Prob(1//100)
    else zero(Prob)
    end
end

@inline function rel_prob(star::Star, monster_star::Star)::Prob
    _rel_prob(star - monster_star)
end

@inline function _rel_prob_bonus(d::Int8)::ProbBonus
    if d < 0x1 zero(ProbBonus)
    elseif d == 0x1 ProbBonus(15//100)
    elseif d == 0x2 ProbBonus(7//100)
    elseif d == 0x3 ProbBonus(3//100)
    elseif d == 0x4 ProbBonus(1//1000)
    else zero(ProbBonus)
    end
end

@inline function rel_prob_bonus(star::Star, monster_star::Star)::ProbBonus
    _rel_prob_bonus(star - monster_star)
end

struct HeroUpgradeResult
    hero::Hero
    n_used_cards::Int
end
show(io::IO, x::HeroUpgradeResult) =
    print(io, "HUR($(x.hero), $(x.n_used_cards) cards used)")
iterate(hur::HeroUpgradeResult, s=1) =
    if s == 1 (hur.hero, 2)
    elseif s == 2 (hur.n_used_cards, 3)
    else nothing
    end

function do_hero_upgrade(
    up::HeroUpgrade, cards::Vector{Card}, results::Vector{Bool};
    prob_bonus::ProbBonus=zero(ProbBonus)
)::NamedTuple{(:hero, :prob_bonus), Tuple{Hero, ProbBonus}}
    (hero, target) = up
    (rank, star, lvl) = hero
    (_, target_star, target_lvl) = target
    if lvl == star_cap(star) && star < target_star
        star = inc(star)
    end
    for (card, result) in zip(cards, results)
        monster_star::Star = card.star
        if result
            prob_bonus = zero(ProbBonus)
            lvl += one(UpgradeLvl)
            if lvl == star_cap(star) && star < target_star
                star = inc(star)
            end
        else
            prob_bonus += rel_prob_bonus(star, monster_star)
        end
    end
    return (hero=Hero(rank, star, lvl), prob_bonus=prob_bonus)
end

function simulate_hero_upgrade(
    up::HeroUpgrade, cards;
    ignore_low_star_cards::Bool=false
)::HeroUpgradeResult
    (hero, target) = up
    (rank, star, lvl) = hero
    (_, target_star, target_lvl) = target
    prob_bonus::ProbBonus = zero(ProbBonus)
    if isempty(cards) || lvl >= target_lvl
        return HeroUpgradeResult(hero, 0)
    end
    if lvl == star_cap(star)
        star = inc(star)
    end
    n_used_cards::Int = 0
    for card in cards
        monster_star::Star = card.star
        if star - monster_star > 0x4
            n_used_cards += 1
            ignore_low_star_cards && continue
            error("cannot use low star monster cards")
        end
        prob::Prob = min(rel_prob(star, monster_star) + prob_bonus, one(Prob))
        result::Int = rand(Bernoulli(prob), 1)[]
        n_used_cards += 1
        if result == 1
            prob_bonus = zero(ProbBonus)
            lvl += one(UpgradeLvl)
            if lvl == target_lvl
                return HeroUpgradeResult(target, n_used_cards)
            elseif lvl == star_cap(star)
                star = inc(star)
            end
        else
            prob_bonus += rel_prob_bonus(star, monster_star)
        end
    end
    return HeroUpgradeResult(Hero(rank, star, lvl), n_used_cards)
end

const Price = Float32
const Prices{T} = Dict{T, Price}
const Progress = Float32 # 100(target lvl - current hero lvl)/(target lvl - hero0 lvl)
const AverageCount = Float32
const Efficiency = Float32 # progress % / price
const CardPrices = Prices{Card}
const Stat = NamedTuple{(:efficiency, :progress, :price), Tuple{Efficiency, Progress, Price}}

function expected_stat(
    up::HeroUpgrade, cards, prices::CardPrices;
    n_attempts::Int=10_000,
    ignore_low_star_cards::Bool=false
)::Stat
    ((_,_,lvl0), (_,_,lvl1)) = up.target
    average_efficiency::Efficiency = zero(Efficiency)
    average_progress::Progress = zero(Progress)
    average_price::Price = zero(Price)
    for attempt in 1:n_attempts
        ((_,_,lvl), n) = simulate_hero_upgrade(up, cards, ignore_low_star_cards=ignore_low_star_cards)
        price::Price = sum(map(card -> prices[card], take(cards, n)))
        progress::Progress = 100*(lvl-lvl0)/(lvl1-lvl0)
        average_efficiency += progress/(price*n_attempts)
        average_progress += progress/n_attempts
        average_price += price/n_attempts
    end
    return Stat((average_efficiency, average_progress, average_price))
end

function expected_n_cards(
    up::HeroUpgrade, card::Card;
    n_attempts::Int=10_000,
    ignore_low_star_cards::Bool=false
)::AverageCount
    (efficiency, progress, n_cards) = expected_stat(
        up, cycle([card]),
        prices=CardPrices(card => one(Price)),
        n_attempts=n_attempts,
        ignore_low_star_cards=ignore_low_star_cards
    )
    return n_cards
end

# NOTE: priceXdNL ≈ N priceXd1L -- experimental fact
const price0d1L = 1.0  # * 5MC : H50 -> H51
const price1d1L = 1.71 # * 4MC : H50 -> H51
const price2d1L = 2.86 # * 3MC : H50 -> H51
const price3d1L = 5.23 # * 2MC : H50 -> H51
const price4d1L = 31.8 # * 1MC : H50 -> H51
# USE: expected_n_cards(50H ↦ 59H, ?MC; n_attempts=1_000_000)/9

@inline function d_price(d::Int8)::Price
    if d <= 0x0 1/price0d1L
    elseif d == 0x1 1/price1d1L
    elseif d == 0x2 1/price2d1L
    elseif d == 0x3 1/price3d1L
    elseif d == 0x4 1/price4d1L
    else zero(Price)
    end
end

const Many{T} = Dict{T, Int}

function prices_of(ups::Many{HeroUpgrade})::CardPrices
    ps::CardPrices = CardPrices((card, zero(Price)) for card in all_cards)
    for (up, n) in ups
        (hero, target) = up
        if hero.upgrade < target.upgrade
            before_star_cap::UpgradeLvl = star_cap(hero.star) - hero.upgrade
            for card in all_cards
                ps[card] += before_star_cap * d_price(hero.star - card.star)
            end
            n_star_incs::UInt8 = (target.upgrade - star_cap(hero.star)) ÷ 0x3
            for d_star = 0x1:n_star_incs
                star::Star = inc(hero.star, d_star)
                for card in all_cards
                    ps[card] += 0x3 * d_price(star - card.star)
                end
            end
        end
    end
    return ps
end

const prices_example = prices_of(Dict(
    30A > 33A => 20,
    40A > 46A => 15,
    50A > 59A => 5,
    40S > 59S => 10,
    40S > 612S => 5,
    60S > 612S => 8,
))

# eval best infinite stream of cards
function simple_chooser(
    up::HeroUpgrade, prices::CardPrices;
    n_attempts::Int=1_000
)::PriorityQueue{Card, Tuple{Price, AverageCount}}
    pq::PriorityQueue{Card, Tuple{Price, AverageCount}} = PriorityQueue()
    for (card, price) in prices
        up.target.star == Star(0x6) && card == 1MC && continue # low star MC cannot upgrade high star Hero
        n::AverageCount = expected_n_cards(up, card, n_attempts=n_attempts)
        enqueue!(pq, card => (n * price, n))
    end
    return result
end

# TODO: cur. count + honor shop 1-3 MC => prices

@inline function many_cards(mc1::Int=0, mc2::Int=0, mc3::Int=0, mc4::Int=0, mc5::Int=0, mc6::Int=0)::Many{Card}
    return Many{Card}(1MC=>mc1, 2MC=>mc2, 3MC=>mc3, 4MC=>mc4, 5MC=>mc5, 6MC=>mc6)
end

@inline function many_permutations(xs::Many{X}, n::Int) where {X}
    return multiset_permutations(collect(keys(xs)), collect(map(k -> k > n ? n : k, values(xs))), n)
end

const cards_example = many_cards(6, 108, 221, 24, 6)

const Distrib{T} = Dict{T, Prob}

function average(::Type{R}, f::Function, distrib::Distrib{X}) where {X, R <: Real}
    av_f::R = zero(R)
    for (x, p) in distrib
        av_f += p * (f(x)::R)
    end
    return av_f
end

const PHURDistrib = Vector{Tuple{HeroUpgradeResult, ProbBonus, Prob}}

function hero_upgrade_distrib(
    up::HeroUpgrade, cards::Vector{Card};
    prob_bonus::ProbBonus=zero(ProbBonus),
    prob_threshold::Prob=Prob(1e-5)
)::Distrib{HeroUpgradeResult}
    phurd = hero_upgrade_distrib(
        up, list(cards...),
        PHURDistrib([(HeroUpgradeResult(up.hero, 0), prob_bonus, one(Prob))]),
        prob_threshold=prob_threshold
    )
    d::Distrib{HeroUpgradeResult}=Distrib{HeroUpgradeResult}()
    for (hur, pb, p) in phurd
        if haskey(d, hur)
            d[hur] += p
        else
            d[hur] = p
        end
    end
    return d
end

function hero_upgrade_distrib(
    up::HeroUpgrade, cards::LinkedList{Card}, distrib::PHURDistrib;
    prob_threshold::Prob=Prob(1e-5)
)::PHURDistrib
    isempty(cards) && return distrib
    target::Hero = up.target
    card::Card = cards.head
    card_star::Star = card.star
    next_cards::LinkedList{Card} = cards.tail
    next_distrib::PHURDistrib = PHURDistrib()
    for (hur, pb, p) in distrib
        hur.hero.star - card.star >= 0x5 && continue # low mc cannot be used for high star hero
        if hur.hero.upgrade == up.target.upgrade
            push!(next_distrib, (hur, pb, p))
        else
            (hero, n_used_cards) = hur
            prob::Prob = min(rel_prob(hero.star, card_star) + pb, one(Prob))
            good_prob::Prob = p * prob
            if good_prob > prob_threshold
                next_hero = inc_to(hero, target)
                good = (HeroUpgradeResult(next_hero, n_used_cards + 1), zero(ProbBonus), good_prob)
                push!(next_distrib, good)
            end
            bad_prob::Prob = p - good_prob
            if bad_prob > prob_threshold
                bad = (HeroUpgradeResult(hero, n_used_cards + 1), pb + rel_prob_bonus(hero.star, card_star), bad_prob)
                push!(next_distrib, bad)
            end
        end
    end
    return hero_upgrade_distrib(up, next_cards, next_distrib, prob_threshold=prob_threshold)
end

struct HeroUpgradeStat
    up::HeroUpgrade
    cards::Vector{Card}
    distrib::Distrib{HeroUpgradeResult}
    prices::Union{Missing, CardPrices}
    HeroUpgradeStat(
        up::HeroUpgrade, cards::Vector{Card},
        prices::Union{Missing, CardPrices}=missing,
        initial_prob_bonus::ProbBonus=zero(ProbBonus)
    ) =
        new(up, cards, hero_upgrade_distrib(up, cards, prob_bonus=initial_prob_bonus), prices)
end
function show(io::IO, hus::HeroUpgradeStat)
    up::HeroUpgrade = hus.up
    cards::Vector{Card} = hus.cards
    distrib::Distrib{HeroUpgradeResult} = hus.distrib
    prices = hus.prices
    println(io, "HUS ($up) by $cards:")
    (hero, target) = up
    average_progress::Progress = zero(Progress)
    average_efficiency::Efficiency = zero(Efficiency)
    for ((h, n), p) in sort(collect(distrib), by=kv -> kv.first.hero.upgrade, rev=true)
        progress = (h.upgrade - hero.upgrade)/(target.upgrade - hero.upgrade)
        if prices === missing
            efficiency = Efficiency(100progress/n)
            efficiency_part = ""
        else
            efficiency = Efficiency(100progress/sum(map(c->prices[c], take(cards, n))))
            efficiency_part = "with efficiency $(round(efficiency, sigdigits=3)) "
        end
        p100 = round(100p, sigdigits=5)
        progress100 = round(100progress, sigdigits=4)
        println(io, "    $(p100)%\t=> H $(h.star) $(Int(h.upgrade))/$(Int(target.upgrade))L (progress $(progress100)%, $efficiency_part$n cards used)")
        average_progress += p*100*progress
        average_efficiency += p*efficiency
    end
    avp = round(average_progress, sigdigits=4)
    avl = round(average_progress*target.upgrade/100, sigdigits=4)
    ave = round(average_efficiency, sigdigits=4)
    print(io, "av. progress = $avp% = $(avl)L, av. efficiency = $ave")
end
function efficiency_of(hus::HeroUpgradeStat)::Efficiency
    ((_,_,lvl0),(_,_,lvl1)) = hus.up
    if hus.prices === missing
        return average(Efficiency, (((_,_,lvl),n),)->Efficiency(100(lvl-lvl0)/(n*(lvl1-lvl0))), hus.distrib)
    else
        return average(
            Efficiency,
            (((_,_,lvl),n),) -> Efficiency(100(lvl-lvl0)/((lvl1-lvl0) * sum(map(c->hus.prices[c], take(hus.cards, n))))),
            hus.distrib
        )
    end
end
function progress_of(hus::HeroUpgradeStat)::Progress
    ((_,_,lvl0),(_,_,lvl1)) = hus.up
    return average(Progress, (((_,_,lvl),n),)->Progress(100(lvl-lvl0)/(lvl1-lvl0)), hus.distrib)
end
function price_of(hus::HeroUpgradeStat)::Price
    hus.prices === missing && error("cannot compute av. price without card prices")
    return average(Price, ((_,n),)->Price(sum(map(c->hus.prices[c], take(hus.cards, n)))), hus.distrib)
end

function best_sequences(
    up::HeroUpgrade, prices::CardPrices, cards::Many{Card};
    prob_bonus::ProbBonus=zero(ProbBonus),
    look_ahead::Int=7,
    n_variants::Int=10
)::PriorityQueue{Vector{Card}, Tuple{Efficiency, Progress}}
    (hero, target) = up
    # keeps max n_variants best sequences in reversed order
    pq::PriorityQueue{Tuple{Vector{Card}, Progress}, Efficiency} = PriorityQueue()
    for sequence in many_permutations(cards, look_ahead)
        hus = HeroUpgradeStat(up, sequence, prices, prob_bonus)
        efficiency = efficiency_of(hus)
        progress = progress_of(hus)
        price = price_of(hus)
        if length(pq) < n_variants
            enqueue!(pq, (sequence, progress) => efficiency)
        elseif peek(pq).second < efficiency
            dequeue!(pq)
            enqueue!(pq, (sequence, progress) => efficiency)
        end
    end
    return PriorityQueue(seq => (efficiency, progress) for ((seq, progress), efficiency) in pq)
end

function best_online_answers(
    up::HeroUpgrade, prices::CardPrices, cards::Many{Card};
    prob_bonus::ProbBonus=zero(ProbBonus),
    depth::Int=7,
)::PriorityQueue{Card, Efficiency}
    up.hero.upgrade < up.target.upgrade || error("nothing to upgrade")
    existing_cards = Dict(c => n for (c, n) in cards if n > 0)
    pq = PriorityQueue{Card, Efficiency}()
    for card in keys(existing_cards)
        # TODO: collect 'em in pq
    end
    (card, eff) = _best_online_answer(
        up, prices, existing_cards,
        prob_bonus=prob_bonus,
        depth=depth
    )
    efficiency::Efficiency = eff/(up.target.upgrade - up.hero.upgrade)
    return (card, efficiency)
end

const DUpgradeLvl = UpgradeLvl

 # NOTE: another definition of efficiency: d_lvl/price
function _best_online_answer(
    up::HeroUpgrade, prices::CardPrices, cards::Many{Card};
    prob_bonus::ProbBonus=zero(ProbBonus),
    depth::Int=7,
    d_lvl::DUpgradeLvl=zero(DUpgradeLvl),
    price::Price=zero(Price)
)::Tuple{Union{Missing, Card}, Efficiency}
    all(n > 0 for (c, n) in cards) || error("contains cards with 0 count -- they should be deleted")
    (hero, target) = up
    if depth == 0 || isempty(cards) || hero.upgrade == target.upgrade
        return (missing, d_lvl/price)
    end
    best_card::Union{Missing, Card} = missing
    best_card_efficiency::Efficiency = -1 # less than any sane efficiency
    for card in keys(cards)
        next_cards = Many{Card}(cards)
        card_count = next_cards[card]
        if card_count == 1
            delete!(next_cards, card)
        else
            next_cards[card] = card_count - 1
        end
        success_prob::Prob = min(rel_prob(hero.star, card.star) + prob_bonus, one(Prob))
        (rank, star, lvl) = hero
        next_lvl = lvl + one(DUpgradeLvl)
        next_price = price + prices[card]
        if lvl == star_cap(star) && star < target.star
            star = inc(star)
        end
        next_d_lvl = d_lvl + one(DUpgradeLvl)
        if next_lvl == target.upgrade
            success_efficiency = next_d_lvl/next_price
        else
            success_hero = Hero(rank, star, next_lvl)
            (_, success_efficiency) = _best_online_answer(
                HeroUpgrade(success_hero, target), prices, next_cards,
                prob_bonus=zero(ProbBonus), depth=depth-1,
                d_lvl=next_d_lvl,
                price=next_price
            )
        end
        fail_prob::Prob = 1 - success_prob
        fail_prob_bonus = prob_bonus + rel_prob_bonus(hero.star, card.star)
        (_, fail_efficiency) = _best_online_answer(
            up, prices, next_cards,
            prob_bonus=fail_prob_bonus, depth=depth-1,
            d_lvl=d_lvl,
            price=next_price
        )
        expected_efficiency::Efficiency = success_prob*success_efficiency + fail_prob*fail_efficiency
        if expected_efficiency > best_card_efficiency
            best_card = card
            best_card_efficiency = expected_efficiency
        end
    end
    return (best_card::Card, best_card_efficiency)
end

function online_chooser(
    up::HeroUpgrade, prices::CardPrices, cards::Many{Card};
    prob_bonus::ProbBonus=zero(ProbBonus),
    look_ahead::Int=7,
    n_variants::Int=3
)::Many{Card}
    cmd_prompt = "\$ "
    show_best_sequences_prefix = ""
    show_best_card_prefix = "1"
    show_status_prefix = "!"
    cards_prefix = ":"
    look_ahead_prefix = "/"
    n_variants_prefix = "*"
    choose_variant_prefix = "#"
    continue_upgrade_prefix = ">"
    end_prefix = "."
    success_symbol = "+"
    fail_symbol = "-"
    bests::Union{Missing, PriorityQueue{Vector{Card}, Tuple{Efficiency, Progress}}} = missing
    ended::Bool = false
    @inline print_status() =
        println("$up (prob_bonus = $(round(100*prob_bonus, sigdigits=4))%)")
    @inline unrecognizable(word) =
        println("Unrecognizable word \"$word\"")
    print_status()
    while !ended
        try
            print(cmd_prompt)
            s = readline()
            if s == show_best_sequences_prefix
                if up.hero.upgrade == up.target.upgrade
                    println("Nothing to upgrade")
                    continue
                end
                bests = PriorityQueue(Base.Order.Reverse, @time best_sequences(
                    up, prices, cards,
                    prob_bonus=prob_bonus, look_ahead=look_ahead, n_variants=n_variants
                ))
                println("bests = ")
                for (i, (sequence, (efficiency, progress100))) in enumerate(bests)
                    println("#$i\t$sequence =>\t$(@sprintf("%.6f", efficiency)),\t+$(@sprintf("%.4f", progress100))%")
                end
            elseif s == show_best_card_prefix
                if up.hero.upgrade == up.target.upgrade
                    println("Nothing to upgrade")
                    continue
                end
                (best_card, best_card_efficiency) = @time best_online_answer(
                    up, prices, cards,
                    prob_bonus=prob_bonus,
                    depth=look_ahead
                )
                println("best = ", best_card, ", efficiency = ", best_card_efficiency)
            elseif s == show_status_prefix
                print_status()
            elseif s == cards_prefix
                println("cards = ")
                for pair in SortedDict(cards)
                    println("  ", pair)
                end
            elseif startswith(s, cards_prefix) # example: : 2=5 6=0 1=102
                for assignment in split(strip(s[2:end]))
                    d_eq_d = match(r"(\d+)=(\d+)", s)
                    if d_eq_d === nothing
                        unrecognizable(s)
                        continue
                    else
                        mc = parse(Int, d_eq_d[1])MC
                        count = parse(Int, d_eq_d[2])
                        cards[mc] = count
                    end
                end
            elseif startswith(s, look_ahead_prefix)
                if s == look_ahead_prefix
                    println("look_ahead = ", look_ahead)
                else
                    look_ahead = parse(Int, strip(s[2:end]))
                end
            elseif startswith(s, n_variants_prefix)
                if s == n_variants_prefix
                    println("n_variants = ", n_variants)
                else
                    n_variants = parse(Int, strip(s[2:end]))
                end
            elseif startswith(s, choose_variant_prefix)
                if up.hero.upgrade == up.target.upgrade
                    println("Nothing to upgrade")
                    continue
                end
                parts = split(s)
                if length(parts[1]) == 1 # custom turn mode: example: # 4-1 3+2 +1
                    used_cards = Vector{Card}()
                    results = Vector{Bool}()
                    for word in parts[2:end]
                        form_n_s_c = match(r"(\d+)([^\d]+)(\d+)", word)
                        form_s_c = match(r"([^\d]+)(\d+)", word)
                        if form_n_s_c !== nothing
                            n_part = form_n_s_c[1]
                            symbol = form_n_s_c[2]
                            card_part = form_n_s_c[3]
                        elseif form_s_c !== nothing
                            n_part = "1"
                            symbol = form_s_c[1]
                            card_part = form_s_c[2]
                        else
                            unrecognizable(word)
                            continue
                        end
                        symbol == success_symbol || symbol == fail_symbol || unrecognizable(word)
                        n = parse(Int, n_part)
                        card = Card(Star(parse(Int, card_part)))
                        result = symbol == success_symbol
                        append!(results, repeat([result], n))
                        append!(used_cards, repeat([card], n))
                        cards[card] -= n
                    end
                    r = do_hero_upgrade(up, used_cards, results, prob_bonus=prob_bonus)
                    up = HeroUpgrade(r.hero, up.target)
                    prob_bonus = r.prob_bonus
                    bests = missing
                    print_status()
                elseif bests === missing
                    println("calculate bests first")
                else # example: #1 + 4- 4+ - -
                    variant_n = parse(Int, parts[1][2:end])
                    (sequence, (efficiency, progress)) = collect(bests)[variant_n]
                    results = Vector{Bool}()
                    for word in parts[2:end]
                        endswith(word, success_symbol) || endswith(word, fail_symbol) || error("Unexpected word \"$word\"")
                        if length(word) == 1
                            push!(results, word == success_symbol)
                        else
                            n = parse(Int, word[1:end-1])
                            result = word[end] == success_symbol
                            append!(results, repeat([result], n))
                        end
                    end
                    used_cards = take(sequence, length(results))
                    for card in used_cards
                        cards[card] -= 1
                    end
                    r = do_hero_upgrade(up, collect(used_cards), results, prob_bonus=prob_bonus)
                    up = HeroUpgrade(r.hero, up.target)
                    prob_bonus = r.prob_bonus
                    bests = missing
                    print_status()
                end
            elseif startswith(s, continue_upgrade_prefix)
                rest = strip(s[2:end])
                new_target::Hero = eval(Meta.parse(rest))
                up = HeroUpgrade(up.hero, new_target)
            elseif s == end_prefix
                ended = true
            else
                println("Unrecognized command \"$s\"")
            end
        catch e
            println("Error occured while parsing/proceeding command:")
            println(e)
            println("Please, try again!")
        end
    end
    return cards
end

# simple_chooser(30A > 33A, example) =
  # MC ☆      => (42.5726, 8.57915)
  # MC ☆☆     => (58.7123, 5.15569)
  # MC ☆☆☆    => (64.9155, 2.99991)
  # MC ☆☆☆☆   => (105.047, 2.99991)
  # MC ☆☆☆☆☆  => (134.312, 2.99991)
  # MC ☆☆☆☆☆☆ => (152.995, 2.99991)

# simple_chooser(40S > 46S, example) =
  # MC ☆      => (155.324, 31.3006)
  # MC ☆☆     => (195.823, 17.1957)
  # MC ☆☆☆☆   => (210.094, 5.99982)
  # MC ☆☆☆    => (222.142, 10.2657)
  # MC ☆☆☆☆☆  => (268.624, 5.99982)
  # MC ☆☆☆☆☆☆ => (305.991, 5.99982)

# simple_chooser(56S > 59S, example) =
  # MC ☆☆☆☆☆  => (134.312, 2.99991)
  # MC ☆☆☆☆☆☆ => (152.995, 2.99991)
  # MC ☆☆     => (179.108, 15.728)
  # MC ☆☆☆☆   => (180.49, 5.1544)
  # MC ☆☆☆    => (185.828, 8.58757)
  # MC ☆      => (473.178, 95.354)

# simple_chooser(60S > 612S, example) =
  # MC ☆☆☆☆☆☆ => (611.982, 11.9996)
  # MC ☆☆☆☆☆  => (916.823, 20.4776)
  # MC ☆☆☆☆   => (1208.01, 34.4982)
  # MC ☆☆☆    => (1360.22, 62.8593)
  # MC ☆☆     => (4355.21, 382.443)
