const Star = UInt8 # 1..6
_format_star(star::Star)::String =
    "$(Int(star))☆"

struct MonsterCard
    star::Star
end
Base.show(io::IO, card::MonsterCard) =
    print(io, "MC $(_format_star(card.star))")

const UpgradeLevel = UInt8 # 0..12

struct Hero
    star::Star
    upgrade::UpgradeLevel
end
Base.show(io::IO, hero::Hero) =
    print(io, "H $(_format_star(hero.star)) $(Int(hero.upgrade))L")

@enum HeroRank::UInt8 A=0x01 S=0x02 SR=0x03

struct RankedHero
    rank::HeroRank
    star::Star
    upgrade::UpgradeLevel
end
Base.show(io::IO, hero::RankedHero) =
    print(io, "H $(hero.rank) $(_format_star(hero.star)) $(Int(hero.upgrade))L")

const Prob = Float16
const ProbBonus = Prob

function _rel_prob(d::UInt8)::Prob
    if d < 0x01 Prob(1)
    elseif d == 0x01 Prob(1/2)
    elseif d == 0x02 Prob(1/4)
    elseif d == 0x03 Prob(1/10)
    elseif d == 0x04 Prob(1/100)
    else Prob(0)
    end
end

function rel_prob(star::Star, card::MonsterCard)::Prob
    _rel_prob(star - card.star)
end

function _rel_prob_bonus(d::UInt8)::ProbBonus
    if d < 0x01 ProbBonus(0)
    elseif d == 0x01 ProbBonus(15/100)
    elseif d == 0x02 ProbBonus(7/100)
    elseif d == 0x03 ProbBonus(3/100)
    elseif d == 0x04 ProbBonus(1/1000)
    else ProbBonus(0)
    end
end

function rel_prob_bonus(star::Star, card::MonsterCard)::ProbBonus
    _rel_prob_bonus(star - card.star)
end
