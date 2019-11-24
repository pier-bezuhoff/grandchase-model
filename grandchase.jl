import Base: *, -, show

@enum Star::UInt8 ☆=0x01 ☆☆ ☆☆☆ ☆☆☆☆ ☆☆☆☆☆ ☆☆☆☆☆☆
*(n::Int, s::Star)::Star =
    Star(n * UInt8(s))
-(s0::Star, s::Star)::Int8 =
    Int8(s0) - Int8(s)
show(io::IO, star::Star) =
    "$(Int(star))☆"

struct MonsterCard
    star::Star
end
MC(i::Int)::MonsterCard =
    MonsterCard(Star(i))
show(io::IO, card::MonsterCard) =
    print(io, "MC $(card.star)")

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
    print(io, "H $(hero.star) $(Int(hero.upgrade))L")

@enum HeroRank::UInt8 A S SR

struct RankedHero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLevel
end
show(io::IO, hero::RankedHero) =
    print(io, "H $(hero.rank) $(hero.star) $(Int(hero.upgrade))L")

# const Prob = Rational{Int} # try/cmp performance
const Prob = Float16
const ProbBonus = Prob

function _rel_prob(d::Int8)::Prob
    if d < 0x01 Prob(1)
    elseif d == 0x01 Prob(1//2)
    elseif d == 0x02 Prob(1//4)
    elseif d == 0x03 Prob(1//10)
    elseif d == 0x04 Prob(1//100)
    else Prob(0)
    end
end

function rel_prob(star::Star, card::MonsterCard)::Prob
    _rel_prob(star - card.star)
end

function _rel_prob_bonus(d::Int8)::ProbBonus
    if d < 0x01 ProbBonus(0)
    elseif d == 0x01 ProbBonus(15//100)
    elseif d == 0x02 ProbBonus(7//100)
    elseif d == 0x03 ProbBonus(3//100)
    elseif d == 0x04 ProbBonus(1//1000)
    else ProbBonus(0)
    end
end

function rel_prob_bonus(star::Star, card::MonsterCard)::ProbBonus
    _rel_prob_bonus(star - card.star)
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
    upgrade::UpgradeLevel
    rest::Vector{MonsterCard}
end

function simulate_hero_upgrade(hero::Hero, target::Hero, cards::Vector{MonsterCard})::HeroUpgradeResult
    @assert target.upgrade > hero.upgrade "target hero level < current level"
    @assert hero.upgrade <= star_cap(hero.star) && target.upgrade <= star_cap(target.star) "level > start cap"
    prob_bonus = ProbBonus(0)
    lvl = hero.upgrade
    star = hero.star
    target_lvl = target.upgrade
    if isempty(cards) || lvl >= target_lvl
        return HeroUpgradeResult(prob_bonus lvl, cards)
    end
    #for card in cards
    #end
    return HeroUpgradeResult(prob_bonus, lvl, Vector{MonsterCard}())
end
