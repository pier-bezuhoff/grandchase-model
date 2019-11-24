using Random
using Distributions

import Base: *, -, show

@enum Star::UInt8 ☆=0x01 ☆☆ ☆☆☆ ☆☆☆☆ ☆☆☆☆☆ ☆☆☆☆☆☆
*(n::Int, s::Star)::Star =
    Star(n * UInt8(s))
-(s0::Star, s::Star)::Int8 =
    Int8(s0) - Int8(s)
inc(star::Star)::Star =
    Star(UInt8(star) + 0x01)
show(io::IO, star::Star) =
    print(io, Int(star), "☆")

struct MonsterCard
    star::Star
end
MC(i::Int)::MonsterCard =
    MonsterCard(Star(i))
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

@enum HeroRank::UInt8 A S SR

struct RankedHero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLevel
end
show(io::IO, hero::RankedHero) =
    print(io, "H ", hero.rank, " ", hero.star, " ", Int(hero.upgrade), "L")

# const Prob = Rational{Int} # try/cmp performance
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
    prob_bonus::ProbBonus
    hero::Hero
    rest::Vector{MonsterCard}
end

function simulate_hero_upgrade(hero::Hero, target::Hero, cards::Vector{MonsterCard})::HeroUpgradeResult
    lvl = hero.upgrade
    star = hero.star
    target_lvl = target.upgrade
    target_star = target.star
    @assert target_lvl < lvl "target hero level < current level"
    @assert lvl <= star_cap(star) && target_lvl <= star_cap(target_star) "level > start cap"
    prob_bonus = zero(ProbBonus)
    if isempty(cards) || lvl >= target_lvl
        return HeroUpgradeResult(prob_bonus, hero, cards)
    end
    if lvl == star_cap(star)
        star = inc(star)
    end
    n_cards = length(cards)
    for i = 1:n_cards
        @inbounds monster_star = cards[i].card
        prob = rel_prob(star, monster_star) + prob_bonus
        result = rand(Bernoulli(prob), 1)[1]
        if result == 1
            prob_bonus = zero(ProbBonus)
            lvl += one(UpgradeLevel)
            if lvl == target_lvl
                return HeroUpgradeResult(prob_bonus, target, cards[i+1:end])
            elseif lvl == star_cap(star)
                star = inc(star)
            end
        else
            prob_bonus += rel_prob_bonus(star, monster_star)
        end
    end
    return HeroUpgradeResult(prob_bonus, Hero(star, lvl), empty(cards))
end
