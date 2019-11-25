using Random
using Distributions
using Base.Iterators

import Base: *, -, show, convert

@enum Star::UInt8 ☆=0x01 ☆☆ ☆☆☆ ☆☆☆☆ ☆☆☆☆☆ ☆☆☆☆☆☆
*(n::Int, s::Star)::Star =
    Star(n * UInt8(s))
-(s0::Star, s::Star)::Int8 =
    Int8(s0) - Int8(s)
inc(star::Star, d::UInt8)::Star =
    Star(UInt8(star) + d)
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

struct Hero
    star::Star # 3..6
    upgrade::UpgradeLvl
    function Hero(star::Star, upgrade::UpgradeLvl)
        UInt8(star) >= 0x03 || error("hero must have >= 3 stars")
        upgrade <= star_cap(star) || error("hero upgrade lvl capped by stars")
        new(star, upgrade)
    end
end
show(io::IO, hero::Hero) =
    print(io, "H ", hero.star, " ", Int(hero.upgrade), "L")
Hero(n_stars::Int, lvl::Int)::Hero =
    Hero(Star(n_stars), UpgradeLvl(lvl))
struct HeroSymbol end
const H = HeroSymbol()
function *(n::Int, ::HeroSymbol)::Hero
    ds = digits(n)
    n_digits = length(ds)
    @assert n_digits >= 2
    return Hero(Star(last(ds)), UpgradeLvl(n % 10 ^ (n_digits - 1)))
end

struct HeroUpgrade
    hero::Hero
    target::Hero
    function HeroUpgrade(hero::Hero, target::Hero)
        target.upgrade > hero.upgrade || error("target hero upgrade < current hero upgrade")
        hero.upgrade <= star_cap(hero.star) && target.upgrade <= star_cap(target.star) || error("upgrade > star cap")
        new(hero, target)
    end
end
show(io::IO, up::HeroUpgrade) =
    print(io, up.hero, " → ", up.target)
→(hero::Hero, target::Hero)::HeroUpgrade =
    HeroUpgrade(hero, target)

@enum HeroRank::UInt8 A S SR

struct RankedHero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLvl
    function RankedHero(rank::HeroRank, star::Star, upgrade::UpgradeLvl)
        UInt8(star) >= 0x03 || error("hero must have >= 3 stars")
        upgrade <= star_cap(star) || error("hero upgrade lvl capped by stars")
        new(rank, star, upgrade)
    end
end
show(io::IO, hero::RankedHero) =
    print(io, "H ", hero.rank, " ", hero.star, " ", Int(hero.upgrade), "L")
function *(n::Int, rank::HeroRank)::RankedHero
    ds = digits(n)
    n_digits = length(ds)
    @assert n_digits >= 2
    return RankedHero(rank, Star(last(ds)), UpgradeLvl(n % 10 ^ (n_digits - 1)))
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

struct HeroUpgradeResult
    hero::Hero
    n_used_cards::Int
end
show(io::IO, x::HeroUpgradeResult) =
    print(io, "HUR($(x.hero), $(x.n_used_cards) cards used)")

function simulate_hero_upgrade(up::HeroUpgrade, target::Hero, cards)::HeroUpgradeResult
    hero::Hero = up.hero
    target::Hero = up.target
    lvl::UpgradeLvl = hero.upgrade
    star::Star = hero.star
    target_lvl::UpgradeLvl = target.upgrade
    target_star::Star = target.star
    target_lvl > lvl || error("target hero level < current level")
    lvl <= star_cap(star) && target_lvl <= star_cap(target_star) || error("level > start cap")
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
# simulate onine methods (batch after batch)

function expected_n_cards(up::HeroUpgrade, cards::Vector{MonsterCard}; n_attempts::Int=10000)::Float64
    cards = cycle(cards)
    average::Float64 = zero(Float64)
    # TODO: try mean([...])
    for attempt in 1:n_attempts
        hur = simulate_hero_upgrade(up, cards)
        average += hur.n_used_cards / n_attempts
    end
    return average
end

function expected_n_cards(up::HeroUpgrade, card::MonsterCard; n_attempts::Int=10000)::Float64
    return expected_n_cards(up, [card]; n_attempts=n_attempts)
end

# NOTE: priceXdNL ≈ N priceXd1L -- experimental fact
const price0d1L = 1.0  # * 5MC : H50 -> H51
const price1d1L = 1.71 # * 4MC : H50 -> H51
const price2d1L = 2.86 # * 3MC : H50 -> H51
const price3d1L = 5.23 # * 2MC : H50 -> H51
const price4d1L = 31.8 # * 1MC : H50 -> H51

function price(d::Int8)::Float16
    if d <= 0x00 1/price0d1L
    elseif d == 0x01 1/price1d1L
    elseif d == 0x02 1/price2d1L
    elseif d == 0x03 1/price3d1L
    elseif d == 0x04 1/price4d1L
    else 0.0
    end
end

function prices(ups::Dict{HeroUpgrade, UInt8})::Dict{MonsterCard, Float16}
    all_cards::Vector{MonsterCard} = MonsterCard.(Star.(1:6))
    ps::Dict{MonsterCard, Float16} = Dict((card, zero(Float16)) for card in all_cards)
    for (up, n) in ups
        hero::Hero = up.hero
        target::Hero = up.target
        if hero.upgrade < target.upgrade
            before_star_cap::UpgradeLvl = star_cap(hero.star) - hero.upgrade
            for card in all_cards
                ps[card] += before_star_cap * price(hero.star - card.star)
            end
            n_star_incs::UInt8 = (target.upgrade - star_cap(hero.star)) ÷ 0x03
            for d_star = 0x01:n_star_incs
                star::Star = inc(hero.star, d_star)
                for card in all_cards
                    ps[card] += 0x03 * price(star - card.star)
                end
            end
        end
    end
    return ps
end

struct Probably{T}
    prob::Prob
    value::T
end
const Distrib{T} = Dict{T, Prob}
