using Random
using Distributions
using Base.Iterators

import Base: *, -, show, convert

@enum Star::UInt8 ☆=0x01 ☆☆ ☆☆☆ ☆☆☆☆ ☆☆☆☆☆ ☆☆☆☆☆☆
*(n::Int, s::Star)::Star =
    Star(n * UInt8(s))
-(s0::Star, s::Star)::Int8 =
    Int8(s0) - Int8(s)
inc(star::Star)::Star =
    Star(UInt8(star) + 0x01)

struct MonsterCard
    star::Star
end
struct MonsterCardSymbol end
const MC = MonsterCardSymbol()
*(i::Int, ::MonsterCardSymbol)::MonsterCard =
    MonsterCard(Star(i))
*(n::Int, card::MonsterCard)::Vector{MonsterCard} =
    fill(card, n)
show(io::IO, card::MonsterCard) =
    print(io, "MC ", card.star)

const UpgradeLevel = UInt8 # 0..12
struct LvlSymbol end
const L = LvlSymbol()
*(n::Int, ::LvlSymbol)::UpgradeLevel =
    UpgradeLevel(n)

struct Hero
    star::Star # 3..6
    upgrade::UpgradeLevel
end
show(io::IO, hero::Hero) =
    print(io, "H ", hero.star, " ", Int(hero.upgrade), "L")
Hero(n_stars::Int, lvl::Int)::Hero =
    Hero(Star(n_stars), UpgradeLevel(lvl))
struct HeroSymbol end
const H = HeroSymbol()
function *(n::Int, ::HeroSymbol)::Hero
    ds = digits(n)
    n_digits = length(ds)
    @assert n_digits >= 2
    return Hero(Star(last(ds)), UpgradeLevel(n % 10 ^ (n_digits - 1)))
end

@enum HeroRank::UInt8 A S SR

struct RankedHero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLevel
end
show(io::IO, hero::RankedHero) =
    print(io, "H ", hero.rank, " ", hero.star, " ", Int(hero.upgrade), "L")
function *(n::Int, rank::HeroRank)::RankedHero
    ds = digits(n)
    n_digits = length(ds)
    @assert n_digits >= 2
    return RankedHero(rank, Star(last(ds)), UpgradeLevel(n % 10 ^ (n_digits - 1)))
end
convert(::Type{Hero}, hero::RankedHero)::Hero =
    Hero(hero.star, hero.upgrade)

#const Prob = Rational{Int} # try/cmp performance
#const Prob = Float32
const Prob = Float16
const ProbBonus = Prob

function _rel_prob(d::Int8)::Prob
    if d < 0x01 Prob(1)
    elseif d == 0x01 Prob(1//2)
    elseif d == 0x02 Prob(1//4)
    elseif d == 0x03 Prob(1//10)
    elseif d == 0x04 Prob(1//100)
    else zero(Prob)
    end
end

function rel_prob(star::Star, monster_star::Star)::Prob
    _rel_prob(star - monster_star)
end

function _rel_prob_bonus(d::Int8)::ProbBonus
    if d < 0x01 zero(ProbBonus)
    elseif d == 0x01 ProbBonus(15//100)
    elseif d == 0x02 ProbBonus(7//100)
    elseif d == 0x03 ProbBonus(3//100)
    elseif d == 0x04 ProbBonus(1//1000)
    else zero(ProbBonus)
    end
end

function rel_prob_bonus(star::Star, monster_star::Star)::ProbBonus
    _rel_prob_bonus(star - monster_star)
end

function star_cap(star::Star)::UpgradeLevel
    if star == ☆☆☆ UpgradeLevel(3)
    elseif star == ☆☆☆☆ UpgradeLevel(6)
    elseif star == ☆☆☆☆☆ UpgradeLevel(9)
    elseif star == ☆☆☆☆☆☆ UpgradeLevel(12)
    else UpgradeLevel(0) # bad
    end
end

struct Probably{T}
    prob::Prob
    value::T
end
const Distrib{T} = Dict{T, Prob}

struct HeroUpgradeResult
    hero::Hero
    n_used_cards::Int
end
show(io::IO, x::HeroUpgradeResult) =
    print(io, "HUR($(x.hero), $(x.n_used_cards) cards used)")

function simulate_hero_upgrade(hero::Hero, target::Hero, cards)::HeroUpgradeResult
    lvl::UpgradeLevel = hero.upgrade
    star::Star = hero.star
    target_lvl::UpgradeLevel = target.upgrade
    target_star::Star = target.star
    @assert target_lvl > lvl "target hero level < current level"
    @assert lvl <= star_cap(star) && target_lvl <= star_cap(target_star) "level > start cap"
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
        prob::Prob = min(rel_prob(star, monster_star) + prob_bonus, one(Prob))
        result::Int = rand(Bernoulli(prob), 1)[]
        n_used_cards += 1
        if result == 1
            prob_bonus = zero(ProbBonus)
            lvl += one(UpgradeLevel)
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

function expected_n_cards(hero::Hero, target::Hero, card::MonsterCard; n_attempts::Int=1000)::Float64
    cards = cycle([card])
    sum::BigInt = 0
    for attempt in 1:n_attempts
        hur = simulate_hero_upgrade(hero, target, cards)
        sum += hur.n_used_cards
    end
    return Float64(sum / n_attempts)
end

function expected_n_cards_f(hero::Hero, target::Hero, card::MonsterCard; n_attempts::Int=1000)::Float64
    cards = cycle([card])
    average::Float64 = zero(Float64)
    for attempt in 1:n_attempts
        hur = simulate_hero_upgrade(hero, target, cards)
        average += hur.n_used_cards / n_attempts
    end
    return average
end

# NOTE: priceXdNL ≈ N priceXd1L -- experimental fact
const price2d3L = 8.6  # * MC(1) : H30 -> H33
const price3d6L = 31.4 # * MC(1) : H40 -> H46
const price2d6L = 17.2 # * MC(2) : H40 -> H46
