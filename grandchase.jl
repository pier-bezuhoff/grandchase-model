using Random
using Distributions
using DataStructures
using Base.Iterators
import Base: *, -, show, convert, isless

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
    print(io, "MC ", card.star)
const all_cards = Card.(Star.(0x1:0x6))
const game_price_MC1 = 5 # in honor shop
const game_price_MC2 = 15
const game_price_MC3 = 45

const UpgradeLvl = UInt8 # 0..12
struct LvlSymbol end
const L = LvlSymbol()
*(n::Int, ::LvlSymbol)::UpgradeLvl =
    UpgradeLvl(n)

function star_cap(star::Star)::UpgradeLvl
    if star == ☆☆☆ UpgradeLvl(3)
    elseif star == ☆☆☆☆ UpgradeLvl(6)
    elseif star == ☆☆☆☆☆ UpgradeLvl(9)
    elseif star == ☆☆☆☆☆☆ UpgradeLvl(12)
    else UpgradeLvl(0) # bad
    end
end

@enum HeroRank::UInt8 H A S SR # 'H' means "any"
struct Hero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLvl
    function Hero(rank::HeroRank, star::Star, upgrade::UpgradeLvl)
        UInt8(star) >= 0x3 || error("hero star >= 3")
        upgrade <= star_cap(star) || error("hero upgrade capped by star")
        rank == H ||
        rank == A ||
        rank == S && UInt8(star) >= 0x4 ||
        rank == SR && UInt8(star) >= 0x5 || error("hero star capped by rank")
        new(rank, star, upgrade)
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

struct HeroUpgrade
    hero::Hero
    target::Hero
    function HeroUpgrade(hero::Hero, target::Hero)
        target.upgrade > hero.upgrade || error("target hero upgrade < current hero upgrade")
        hero.upgrade <= star_cap(hero.star) && target.upgrade <= star_cap(target.star) || error("upgrade > star cap")
        hero.rank == target.rank || error("inconsistent hero rank")
        new(hero, target)
    end
end
show(io::IO, up::HeroUpgrade) =
    print(io, up.hero, " ↦ ", up.target)
↦(hero::Hero, target::Hero)::HeroUpgrade =
    HeroUpgrade(hero, target)

# const Prob = Rational{Int} # try/cmp performance
const Prob = Float32
const ProbBonus = Prob

function _rel_prob(d::Int8)::Prob
    if d < 0x1 Prob(1)
    elseif d == 0x1 Prob(1//2)
    elseif d == 0x2 Prob(1//4)
    elseif d == 0x3 Prob(1//10)
    elseif d == 0x4 Prob(1//100)
    else zero(Prob)
    end
end

function rel_prob(star::Star, monster_star::Star)::Prob
    _rel_prob(star - monster_star)
end

function _rel_prob_bonus(d::Int8)::ProbBonus
    if d < 0x1 zero(ProbBonus)
    elseif d == 0x1 ProbBonus(15//100)
    elseif d == 0x2 ProbBonus(7//100)
    elseif d == 0x3 ProbBonus(3//100)
    elseif d == 0x4 ProbBonus(1//1000)
    else zero(ProbBonus)
    end
end

function rel_prob_bonus(star::Star, monster_star::Star)::ProbBonus
    _rel_prob_bonus(star - monster_star)
end

struct HeroUpgradeResult
    hero::Hero
    n_used_cards::Int
end
show(io::IO, x::HeroUpgradeResult) =
    print(io, "HUR($(x.hero), $(x.n_used_cards) cards used)")

function simulate_hero_upgrade(
    up::HeroUpgrade, cards;
    ignore_low_star_cards::Bool=false
)::HeroUpgradeResult
    hero::Hero = up.hero
    target::Hero = up.target
    lvl::UpgradeLvl = hero.upgrade
    star::Star = hero.star
    target_lvl::UpgradeLvl = target.upgrade
    target_star::Star = target.star
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
    return HeroUpgradeResult(Hero(star, lvl), n_used_cards)
end

const Price = Float32
const Prices{T} = Dict{T, Price}
const CardPrices = Prices{Card}

function expected_price(
    up::HeroUpgrade, cards::Vector{Card};
    prices::CardPrices = CardPrices((card, one(Price)) for card in all_cards),
    n_attempts::Int=10_000,
    ignore_low_star_cards::Bool=false
)::Price
    cards = cycle(cards)
    average::Price = zero(Price)
    for attempt in 1:n_attempts
        hur = simulate_hero_upgrade(up, cards, ignore_low_star_cards=ignore_low_star_cards)
        price::Price = sum(map(card -> prices[card], take(cards, hur.n_used_cards)))
        average += price / n_attempts
    end
    return average
end

function expected_n_cards(
    up::HeroUpgrade, card::Card;
    n_attempts::Int=10_000,
    ignore_low_star_cards::Bool=false
)::Float32
    return expected_price(
        up, [card],
        prices=CardPrices(card => one(Price)),
        n_attempts=n_attempts,
        ignore_low_star_cards=ignore_low_star_cards
    )
end

# NOTE: priceXdNL ≈ N priceXd1L -- experimental fact
const price0d1L = 1.0  # * 5MC : H50 -> H51
const price1d1L = 1.71 # * 4MC : H50 -> H51
const price2d1L = 2.86 # * 3MC : H50 -> H51
const price3d1L = 5.23 # * 2MC : H50 -> H51
const price4d1L = 31.8 # * 1MC : H50 -> H51
# USE: expected_n_cards(50H ↦ 59H, ?MC; n_attempts=1_000_000)/9

function price(d::Int8)::Price
    if d <= 0x0 1/price0d1L
    elseif d == 0x1 1/price1d1L
    elseif d == 0x2 1/price2d1L
    elseif d == 0x3 1/price3d1L
    elseif d == 0x4 1/price4d1L
    else zero(Price)
    end
end

struct Indefinite end # indicates potential infinity
const indefinite = Indefinite()
const Many{T} = Dict{T, Int}
const Much{T} = Dict{T, Union{Int, Indefinite}}

function prices(ups::Many{HeroUpgrade})::CardPrices
    ps::CardPrices = CardPrices((card, zero(Price)) for card in all_cards)
    for (up, n) in ups
        hero::Hero = up.hero
        target::Hero = up.target
        if hero.upgrade < target.upgrade
            before_star_cap::UpgradeLvl = star_cap(hero.star) - hero.upgrade
            for card in all_cards
                ps[card] += before_star_cap * price(hero.star - card.star)
            end
            n_star_incs::UInt8 = (target.upgrade - star_cap(hero.star)) ÷ 0x3
            for d_star = 0x1:n_star_incs
                star::Star = inc(hero.star, d_star)
                for card in all_cards
                    ps[card] += 0x3 * price(star - card.star)
                end
            end
        end
    end
    return ps
end

const example = prices(Dict(
    30A ↦ 33A => 20,
    40A ↦ 46A => 15,
    50A ↦ 59A => 5,
    40S ↦ 59S => 10,
    40S ↦ 612S => 5,
    60S ↦ 612S => 8,
))

# eval best infinite steam of cards
function simple_chooser(up::HeroUpgrade, prices::CardPrices; n_attempts::Int=1_000)::SortedDict{Card, Tuple{Price, Float32}}
    result::SortedDict{Card, Tuple{Price, Float32}} = SortedDict()
    for (card, price) in prices
        up.target.star == Star(0x6) && card == 1MC && continue # low star MC cannot upgrade high star Hero
        n::Float32 = expected_n_cards(up, card, n_attempts=n_attempts)
        result[card] = (n * price, n)
    end
    return result
end

# TODO: cur. count + honor shop 1-3 MC => prices

#=
function online_chooser(
    up::HeroUpgrade, prices::CardPrices, cards::Many{Card},
    look_ahead::Int = 10,
    n_attempts::Int = 100
)
    hero::Hero = up.hero
    target::Hero = up.target
    function next(result::Union{Nothing,Bool} = nothing)::Card
        for cs in combinations of look_ahead cards
            #
        end
        if result === nothing
            #first turn
        else
            #>=2nd
        end
    end
    return next
end
=#

SortedDict(example)

#=
struct Probably{T}
    prob::Prob
    value::T
end
const Distrib{T} = Dict{T, Prob}
=#
