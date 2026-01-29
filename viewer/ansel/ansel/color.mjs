import { toList, CustomType as $CustomType } from "../gleam.mjs";

export class RGBA extends $CustomType {
  constructor(r, g, b, a) {
    super();
    this.r = r;
    this.g = g;
    this.b = b;
    this.a = a;
  }
}
export const Color$RGBA = (r, g, b, a) => new RGBA(r, g, b, a);
export const Color$isRGBA = (value) => value instanceof RGBA;
export const Color$RGBA$r = (value) => value.r;
export const Color$RGBA$0 = (value) => value.r;
export const Color$RGBA$g = (value) => value.g;
export const Color$RGBA$1 = (value) => value.g;
export const Color$RGBA$b = (value) => value.b;
export const Color$RGBA$2 = (value) => value.b;
export const Color$RGBA$a = (value) => value.a;
export const Color$RGBA$3 = (value) => value.a;

export class RGB extends $CustomType {
  constructor(r, g, b) {
    super();
    this.r = r;
    this.g = g;
    this.b = b;
  }
}
export const Color$RGB = (r, g, b) => new RGB(r, g, b);
export const Color$isRGB = (value) => value instanceof RGB;
export const Color$RGB$r = (value) => value.r;
export const Color$RGB$0 = (value) => value.r;
export const Color$RGB$g = (value) => value.g;
export const Color$RGB$1 = (value) => value.g;
export const Color$RGB$b = (value) => value.b;
export const Color$RGB$2 = (value) => value.b;

export class GleamLucy extends $CustomType {}
export const Color$GleamLucy = () => new GleamLucy();
export const Color$isGleamLucy = (value) => value instanceof GleamLucy;

export class GleamNavy extends $CustomType {}
export const Color$GleamNavy = () => new GleamNavy();
export const Color$isGleamNavy = (value) => value instanceof GleamNavy;

export class Maroon extends $CustomType {}
export const Color$Maroon = () => new Maroon();
export const Color$isMaroon = (value) => value instanceof Maroon;

export class DarkRed extends $CustomType {}
export const Color$DarkRed = () => new DarkRed();
export const Color$isDarkRed = (value) => value instanceof DarkRed;

export class Brown extends $CustomType {}
export const Color$Brown = () => new Brown();
export const Color$isBrown = (value) => value instanceof Brown;

export class Firebrick extends $CustomType {}
export const Color$Firebrick = () => new Firebrick();
export const Color$isFirebrick = (value) => value instanceof Firebrick;

export class Crimson extends $CustomType {}
export const Color$Crimson = () => new Crimson();
export const Color$isCrimson = (value) => value instanceof Crimson;

export class Red extends $CustomType {}
export const Color$Red = () => new Red();
export const Color$isRed = (value) => value instanceof Red;

export class Tomato extends $CustomType {}
export const Color$Tomato = () => new Tomato();
export const Color$isTomato = (value) => value instanceof Tomato;

export class Coral extends $CustomType {}
export const Color$Coral = () => new Coral();
export const Color$isCoral = (value) => value instanceof Coral;

export class IndianRed extends $CustomType {}
export const Color$IndianRed = () => new IndianRed();
export const Color$isIndianRed = (value) => value instanceof IndianRed;

export class LightCoral extends $CustomType {}
export const Color$LightCoral = () => new LightCoral();
export const Color$isLightCoral = (value) => value instanceof LightCoral;

export class DarkSalmon extends $CustomType {}
export const Color$DarkSalmon = () => new DarkSalmon();
export const Color$isDarkSalmon = (value) => value instanceof DarkSalmon;

export class Salmon extends $CustomType {}
export const Color$Salmon = () => new Salmon();
export const Color$isSalmon = (value) => value instanceof Salmon;

export class LightSalmon extends $CustomType {}
export const Color$LightSalmon = () => new LightSalmon();
export const Color$isLightSalmon = (value) => value instanceof LightSalmon;

export class OrangeRed extends $CustomType {}
export const Color$OrangeRed = () => new OrangeRed();
export const Color$isOrangeRed = (value) => value instanceof OrangeRed;

export class DarkOrange extends $CustomType {}
export const Color$DarkOrange = () => new DarkOrange();
export const Color$isDarkOrange = (value) => value instanceof DarkOrange;

export class Orange extends $CustomType {}
export const Color$Orange = () => new Orange();
export const Color$isOrange = (value) => value instanceof Orange;

export class Gold extends $CustomType {}
export const Color$Gold = () => new Gold();
export const Color$isGold = (value) => value instanceof Gold;

export class DarkGoldenRod extends $CustomType {}
export const Color$DarkGoldenRod = () => new DarkGoldenRod();
export const Color$isDarkGoldenRod = (value) => value instanceof DarkGoldenRod;

export class GoldenRod extends $CustomType {}
export const Color$GoldenRod = () => new GoldenRod();
export const Color$isGoldenRod = (value) => value instanceof GoldenRod;

export class PaleGoldenRod extends $CustomType {}
export const Color$PaleGoldenRod = () => new PaleGoldenRod();
export const Color$isPaleGoldenRod = (value) => value instanceof PaleGoldenRod;

export class DarkKhaki extends $CustomType {}
export const Color$DarkKhaki = () => new DarkKhaki();
export const Color$isDarkKhaki = (value) => value instanceof DarkKhaki;

export class Khaki extends $CustomType {}
export const Color$Khaki = () => new Khaki();
export const Color$isKhaki = (value) => value instanceof Khaki;

export class Olive extends $CustomType {}
export const Color$Olive = () => new Olive();
export const Color$isOlive = (value) => value instanceof Olive;

export class Yellow extends $CustomType {}
export const Color$Yellow = () => new Yellow();
export const Color$isYellow = (value) => value instanceof Yellow;

export class YellowGreen extends $CustomType {}
export const Color$YellowGreen = () => new YellowGreen();
export const Color$isYellowGreen = (value) => value instanceof YellowGreen;

export class DarkOliveGreen extends $CustomType {}
export const Color$DarkOliveGreen = () => new DarkOliveGreen();
export const Color$isDarkOliveGreen = (value) =>
  value instanceof DarkOliveGreen;

export class OliveDrab extends $CustomType {}
export const Color$OliveDrab = () => new OliveDrab();
export const Color$isOliveDrab = (value) => value instanceof OliveDrab;

export class LawnGreen extends $CustomType {}
export const Color$LawnGreen = () => new LawnGreen();
export const Color$isLawnGreen = (value) => value instanceof LawnGreen;

export class Chartreuse extends $CustomType {}
export const Color$Chartreuse = () => new Chartreuse();
export const Color$isChartreuse = (value) => value instanceof Chartreuse;

export class GreenYellow extends $CustomType {}
export const Color$GreenYellow = () => new GreenYellow();
export const Color$isGreenYellow = (value) => value instanceof GreenYellow;

export class DarkGreen extends $CustomType {}
export const Color$DarkGreen = () => new DarkGreen();
export const Color$isDarkGreen = (value) => value instanceof DarkGreen;

export class Green extends $CustomType {}
export const Color$Green = () => new Green();
export const Color$isGreen = (value) => value instanceof Green;

export class ForestGreen extends $CustomType {}
export const Color$ForestGreen = () => new ForestGreen();
export const Color$isForestGreen = (value) => value instanceof ForestGreen;

export class Lime extends $CustomType {}
export const Color$Lime = () => new Lime();
export const Color$isLime = (value) => value instanceof Lime;

export class LimeGreen extends $CustomType {}
export const Color$LimeGreen = () => new LimeGreen();
export const Color$isLimeGreen = (value) => value instanceof LimeGreen;

export class LightGreen extends $CustomType {}
export const Color$LightGreen = () => new LightGreen();
export const Color$isLightGreen = (value) => value instanceof LightGreen;

export class PaleGreen extends $CustomType {}
export const Color$PaleGreen = () => new PaleGreen();
export const Color$isPaleGreen = (value) => value instanceof PaleGreen;

export class DarkSeaGreen extends $CustomType {}
export const Color$DarkSeaGreen = () => new DarkSeaGreen();
export const Color$isDarkSeaGreen = (value) => value instanceof DarkSeaGreen;

export class MediumSpringGreen extends $CustomType {}
export const Color$MediumSpringGreen = () => new MediumSpringGreen();
export const Color$isMediumSpringGreen = (value) =>
  value instanceof MediumSpringGreen;

export class SpringGreen extends $CustomType {}
export const Color$SpringGreen = () => new SpringGreen();
export const Color$isSpringGreen = (value) => value instanceof SpringGreen;

export class SeaGreen extends $CustomType {}
export const Color$SeaGreen = () => new SeaGreen();
export const Color$isSeaGreen = (value) => value instanceof SeaGreen;

export class MediumAquaMarine extends $CustomType {}
export const Color$MediumAquaMarine = () => new MediumAquaMarine();
export const Color$isMediumAquaMarine = (value) =>
  value instanceof MediumAquaMarine;

export class MediumSeaGreen extends $CustomType {}
export const Color$MediumSeaGreen = () => new MediumSeaGreen();
export const Color$isMediumSeaGreen = (value) =>
  value instanceof MediumSeaGreen;

export class LightSeaGreen extends $CustomType {}
export const Color$LightSeaGreen = () => new LightSeaGreen();
export const Color$isLightSeaGreen = (value) => value instanceof LightSeaGreen;

export class DarkSlateGray extends $CustomType {}
export const Color$DarkSlateGray = () => new DarkSlateGray();
export const Color$isDarkSlateGray = (value) => value instanceof DarkSlateGray;

export class Teal extends $CustomType {}
export const Color$Teal = () => new Teal();
export const Color$isTeal = (value) => value instanceof Teal;

export class DarkCyan extends $CustomType {}
export const Color$DarkCyan = () => new DarkCyan();
export const Color$isDarkCyan = (value) => value instanceof DarkCyan;

export class Aqua extends $CustomType {}
export const Color$Aqua = () => new Aqua();
export const Color$isAqua = (value) => value instanceof Aqua;

export class Cyan extends $CustomType {}
export const Color$Cyan = () => new Cyan();
export const Color$isCyan = (value) => value instanceof Cyan;

export class LightCyan extends $CustomType {}
export const Color$LightCyan = () => new LightCyan();
export const Color$isLightCyan = (value) => value instanceof LightCyan;

export class DarkTurquoise extends $CustomType {}
export const Color$DarkTurquoise = () => new DarkTurquoise();
export const Color$isDarkTurquoise = (value) => value instanceof DarkTurquoise;

export class Turquoise extends $CustomType {}
export const Color$Turquoise = () => new Turquoise();
export const Color$isTurquoise = (value) => value instanceof Turquoise;

export class MediumTurquoise extends $CustomType {}
export const Color$MediumTurquoise = () => new MediumTurquoise();
export const Color$isMediumTurquoise = (value) =>
  value instanceof MediumTurquoise;

export class PaleTurquoise extends $CustomType {}
export const Color$PaleTurquoise = () => new PaleTurquoise();
export const Color$isPaleTurquoise = (value) => value instanceof PaleTurquoise;

export class AquaMarine extends $CustomType {}
export const Color$AquaMarine = () => new AquaMarine();
export const Color$isAquaMarine = (value) => value instanceof AquaMarine;

export class PowderBlue extends $CustomType {}
export const Color$PowderBlue = () => new PowderBlue();
export const Color$isPowderBlue = (value) => value instanceof PowderBlue;

export class CadetBlue extends $CustomType {}
export const Color$CadetBlue = () => new CadetBlue();
export const Color$isCadetBlue = (value) => value instanceof CadetBlue;

export class SteelBlue extends $CustomType {}
export const Color$SteelBlue = () => new SteelBlue();
export const Color$isSteelBlue = (value) => value instanceof SteelBlue;

export class CornFlowerBlue extends $CustomType {}
export const Color$CornFlowerBlue = () => new CornFlowerBlue();
export const Color$isCornFlowerBlue = (value) =>
  value instanceof CornFlowerBlue;

export class DeepSkyBlue extends $CustomType {}
export const Color$DeepSkyBlue = () => new DeepSkyBlue();
export const Color$isDeepSkyBlue = (value) => value instanceof DeepSkyBlue;

export class DodgerBlue extends $CustomType {}
export const Color$DodgerBlue = () => new DodgerBlue();
export const Color$isDodgerBlue = (value) => value instanceof DodgerBlue;

export class LightBlue extends $CustomType {}
export const Color$LightBlue = () => new LightBlue();
export const Color$isLightBlue = (value) => value instanceof LightBlue;

export class SkyBlue extends $CustomType {}
export const Color$SkyBlue = () => new SkyBlue();
export const Color$isSkyBlue = (value) => value instanceof SkyBlue;

export class LightSkyBlue extends $CustomType {}
export const Color$LightSkyBlue = () => new LightSkyBlue();
export const Color$isLightSkyBlue = (value) => value instanceof LightSkyBlue;

export class MidnightBlue extends $CustomType {}
export const Color$MidnightBlue = () => new MidnightBlue();
export const Color$isMidnightBlue = (value) => value instanceof MidnightBlue;

export class Navy extends $CustomType {}
export const Color$Navy = () => new Navy();
export const Color$isNavy = (value) => value instanceof Navy;

export class DarkBlue extends $CustomType {}
export const Color$DarkBlue = () => new DarkBlue();
export const Color$isDarkBlue = (value) => value instanceof DarkBlue;

export class MediumBlue extends $CustomType {}
export const Color$MediumBlue = () => new MediumBlue();
export const Color$isMediumBlue = (value) => value instanceof MediumBlue;

export class Blue extends $CustomType {}
export const Color$Blue = () => new Blue();
export const Color$isBlue = (value) => value instanceof Blue;

export class RoyalBlue extends $CustomType {}
export const Color$RoyalBlue = () => new RoyalBlue();
export const Color$isRoyalBlue = (value) => value instanceof RoyalBlue;

export class BlueViolet extends $CustomType {}
export const Color$BlueViolet = () => new BlueViolet();
export const Color$isBlueViolet = (value) => value instanceof BlueViolet;

export class Indigo extends $CustomType {}
export const Color$Indigo = () => new Indigo();
export const Color$isIndigo = (value) => value instanceof Indigo;

export class DarkSlateBlue extends $CustomType {}
export const Color$DarkSlateBlue = () => new DarkSlateBlue();
export const Color$isDarkSlateBlue = (value) => value instanceof DarkSlateBlue;

export class SlateBlue extends $CustomType {}
export const Color$SlateBlue = () => new SlateBlue();
export const Color$isSlateBlue = (value) => value instanceof SlateBlue;

export class MediumSlateBlue extends $CustomType {}
export const Color$MediumSlateBlue = () => new MediumSlateBlue();
export const Color$isMediumSlateBlue = (value) =>
  value instanceof MediumSlateBlue;

export class MediumPurple extends $CustomType {}
export const Color$MediumPurple = () => new MediumPurple();
export const Color$isMediumPurple = (value) => value instanceof MediumPurple;

export class DarkMagenta extends $CustomType {}
export const Color$DarkMagenta = () => new DarkMagenta();
export const Color$isDarkMagenta = (value) => value instanceof DarkMagenta;

export class DarkViolet extends $CustomType {}
export const Color$DarkViolet = () => new DarkViolet();
export const Color$isDarkViolet = (value) => value instanceof DarkViolet;

export class DarkOrchid extends $CustomType {}
export const Color$DarkOrchid = () => new DarkOrchid();
export const Color$isDarkOrchid = (value) => value instanceof DarkOrchid;

export class MediumOrchid extends $CustomType {}
export const Color$MediumOrchid = () => new MediumOrchid();
export const Color$isMediumOrchid = (value) => value instanceof MediumOrchid;

export class Purple extends $CustomType {}
export const Color$Purple = () => new Purple();
export const Color$isPurple = (value) => value instanceof Purple;

export class Thistle extends $CustomType {}
export const Color$Thistle = () => new Thistle();
export const Color$isThistle = (value) => value instanceof Thistle;

export class Plum extends $CustomType {}
export const Color$Plum = () => new Plum();
export const Color$isPlum = (value) => value instanceof Plum;

export class Violet extends $CustomType {}
export const Color$Violet = () => new Violet();
export const Color$isViolet = (value) => value instanceof Violet;

export class Magenta extends $CustomType {}
export const Color$Magenta = () => new Magenta();
export const Color$isMagenta = (value) => value instanceof Magenta;

export class Fuchsia extends $CustomType {}
export const Color$Fuchsia = () => new Fuchsia();
export const Color$isFuchsia = (value) => value instanceof Fuchsia;

export class Orchid extends $CustomType {}
export const Color$Orchid = () => new Orchid();
export const Color$isOrchid = (value) => value instanceof Orchid;

export class MediumVioletRed extends $CustomType {}
export const Color$MediumVioletRed = () => new MediumVioletRed();
export const Color$isMediumVioletRed = (value) =>
  value instanceof MediumVioletRed;

export class PaleVioletRed extends $CustomType {}
export const Color$PaleVioletRed = () => new PaleVioletRed();
export const Color$isPaleVioletRed = (value) => value instanceof PaleVioletRed;

export class DeepPink extends $CustomType {}
export const Color$DeepPink = () => new DeepPink();
export const Color$isDeepPink = (value) => value instanceof DeepPink;

export class HotPink extends $CustomType {}
export const Color$HotPink = () => new HotPink();
export const Color$isHotPink = (value) => value instanceof HotPink;

export class LightPink extends $CustomType {}
export const Color$LightPink = () => new LightPink();
export const Color$isLightPink = (value) => value instanceof LightPink;

export class Pink extends $CustomType {}
export const Color$Pink = () => new Pink();
export const Color$isPink = (value) => value instanceof Pink;

export class AntiqueWhite extends $CustomType {}
export const Color$AntiqueWhite = () => new AntiqueWhite();
export const Color$isAntiqueWhite = (value) => value instanceof AntiqueWhite;

export class Beige extends $CustomType {}
export const Color$Beige = () => new Beige();
export const Color$isBeige = (value) => value instanceof Beige;

export class Bisque extends $CustomType {}
export const Color$Bisque = () => new Bisque();
export const Color$isBisque = (value) => value instanceof Bisque;

export class BlanchedAlmond extends $CustomType {}
export const Color$BlanchedAlmond = () => new BlanchedAlmond();
export const Color$isBlanchedAlmond = (value) =>
  value instanceof BlanchedAlmond;

export class Wheat extends $CustomType {}
export const Color$Wheat = () => new Wheat();
export const Color$isWheat = (value) => value instanceof Wheat;

export class CornSilk extends $CustomType {}
export const Color$CornSilk = () => new CornSilk();
export const Color$isCornSilk = (value) => value instanceof CornSilk;

export class LemonChiffon extends $CustomType {}
export const Color$LemonChiffon = () => new LemonChiffon();
export const Color$isLemonChiffon = (value) => value instanceof LemonChiffon;

export class LightGoldenRodYellow extends $CustomType {}
export const Color$LightGoldenRodYellow = () => new LightGoldenRodYellow();
export const Color$isLightGoldenRodYellow = (value) =>
  value instanceof LightGoldenRodYellow;

export class LightYellow extends $CustomType {}
export const Color$LightYellow = () => new LightYellow();
export const Color$isLightYellow = (value) => value instanceof LightYellow;

export class SaddleBrown extends $CustomType {}
export const Color$SaddleBrown = () => new SaddleBrown();
export const Color$isSaddleBrown = (value) => value instanceof SaddleBrown;

export class Sienna extends $CustomType {}
export const Color$Sienna = () => new Sienna();
export const Color$isSienna = (value) => value instanceof Sienna;

export class Chocolate extends $CustomType {}
export const Color$Chocolate = () => new Chocolate();
export const Color$isChocolate = (value) => value instanceof Chocolate;

export class Peru extends $CustomType {}
export const Color$Peru = () => new Peru();
export const Color$isPeru = (value) => value instanceof Peru;

export class SandyBrown extends $CustomType {}
export const Color$SandyBrown = () => new SandyBrown();
export const Color$isSandyBrown = (value) => value instanceof SandyBrown;

export class BurlyWood extends $CustomType {}
export const Color$BurlyWood = () => new BurlyWood();
export const Color$isBurlyWood = (value) => value instanceof BurlyWood;

export class Tan extends $CustomType {}
export const Color$Tan = () => new Tan();
export const Color$isTan = (value) => value instanceof Tan;

export class RosyBrown extends $CustomType {}
export const Color$RosyBrown = () => new RosyBrown();
export const Color$isRosyBrown = (value) => value instanceof RosyBrown;

export class Moccasin extends $CustomType {}
export const Color$Moccasin = () => new Moccasin();
export const Color$isMoccasin = (value) => value instanceof Moccasin;

export class NavajoWhite extends $CustomType {}
export const Color$NavajoWhite = () => new NavajoWhite();
export const Color$isNavajoWhite = (value) => value instanceof NavajoWhite;

export class PeachPuff extends $CustomType {}
export const Color$PeachPuff = () => new PeachPuff();
export const Color$isPeachPuff = (value) => value instanceof PeachPuff;

export class MistyRose extends $CustomType {}
export const Color$MistyRose = () => new MistyRose();
export const Color$isMistyRose = (value) => value instanceof MistyRose;

export class LavenderBlush extends $CustomType {}
export const Color$LavenderBlush = () => new LavenderBlush();
export const Color$isLavenderBlush = (value) => value instanceof LavenderBlush;

export class Linen extends $CustomType {}
export const Color$Linen = () => new Linen();
export const Color$isLinen = (value) => value instanceof Linen;

export class OldLace extends $CustomType {}
export const Color$OldLace = () => new OldLace();
export const Color$isOldLace = (value) => value instanceof OldLace;

export class PapayaWhip extends $CustomType {}
export const Color$PapayaWhip = () => new PapayaWhip();
export const Color$isPapayaWhip = (value) => value instanceof PapayaWhip;

export class SeaShell extends $CustomType {}
export const Color$SeaShell = () => new SeaShell();
export const Color$isSeaShell = (value) => value instanceof SeaShell;

export class MintCream extends $CustomType {}
export const Color$MintCream = () => new MintCream();
export const Color$isMintCream = (value) => value instanceof MintCream;

export class SlateGray extends $CustomType {}
export const Color$SlateGray = () => new SlateGray();
export const Color$isSlateGray = (value) => value instanceof SlateGray;

export class LightSlateGray extends $CustomType {}
export const Color$LightSlateGray = () => new LightSlateGray();
export const Color$isLightSlateGray = (value) =>
  value instanceof LightSlateGray;

export class LightSteelBlue extends $CustomType {}
export const Color$LightSteelBlue = () => new LightSteelBlue();
export const Color$isLightSteelBlue = (value) =>
  value instanceof LightSteelBlue;

export class Lavender extends $CustomType {}
export const Color$Lavender = () => new Lavender();
export const Color$isLavender = (value) => value instanceof Lavender;

export class FloralWhite extends $CustomType {}
export const Color$FloralWhite = () => new FloralWhite();
export const Color$isFloralWhite = (value) => value instanceof FloralWhite;

export class AliceBlue extends $CustomType {}
export const Color$AliceBlue = () => new AliceBlue();
export const Color$isAliceBlue = (value) => value instanceof AliceBlue;

export class GhostWhite extends $CustomType {}
export const Color$GhostWhite = () => new GhostWhite();
export const Color$isGhostWhite = (value) => value instanceof GhostWhite;

export class Honeydew extends $CustomType {}
export const Color$Honeydew = () => new Honeydew();
export const Color$isHoneydew = (value) => value instanceof Honeydew;

export class Ivory extends $CustomType {}
export const Color$Ivory = () => new Ivory();
export const Color$isIvory = (value) => value instanceof Ivory;

export class Azure extends $CustomType {}
export const Color$Azure = () => new Azure();
export const Color$isAzure = (value) => value instanceof Azure;

export class Snow extends $CustomType {}
export const Color$Snow = () => new Snow();
export const Color$isSnow = (value) => value instanceof Snow;

export class Black extends $CustomType {}
export const Color$Black = () => new Black();
export const Color$isBlack = (value) => value instanceof Black;

export class DimGray extends $CustomType {}
export const Color$DimGray = () => new DimGray();
export const Color$isDimGray = (value) => value instanceof DimGray;

export class DimGrey extends $CustomType {}
export const Color$DimGrey = () => new DimGrey();
export const Color$isDimGrey = (value) => value instanceof DimGrey;

export class Gray extends $CustomType {}
export const Color$Gray = () => new Gray();
export const Color$isGray = (value) => value instanceof Gray;

export class Grey extends $CustomType {}
export const Color$Grey = () => new Grey();
export const Color$isGrey = (value) => value instanceof Grey;

export class DarkGray extends $CustomType {}
export const Color$DarkGray = () => new DarkGray();
export const Color$isDarkGray = (value) => value instanceof DarkGray;

export class DarkGrey extends $CustomType {}
export const Color$DarkGrey = () => new DarkGrey();
export const Color$isDarkGrey = (value) => value instanceof DarkGrey;

export class Silver extends $CustomType {}
export const Color$Silver = () => new Silver();
export const Color$isSilver = (value) => value instanceof Silver;

export class LightGray extends $CustomType {}
export const Color$LightGray = () => new LightGray();
export const Color$isLightGray = (value) => value instanceof LightGray;

export class LightGrey extends $CustomType {}
export const Color$LightGrey = () => new LightGrey();
export const Color$isLightGrey = (value) => value instanceof LightGrey;

export class Gainsboro extends $CustomType {}
export const Color$Gainsboro = () => new Gainsboro();
export const Color$isGainsboro = (value) => value instanceof Gainsboro;

export class WhiteSmoke extends $CustomType {}
export const Color$WhiteSmoke = () => new WhiteSmoke();
export const Color$isWhiteSmoke = (value) => value instanceof WhiteSmoke;

export class White extends $CustomType {}
export const Color$White = () => new White();
export const Color$isWhite = (value) => value instanceof White;

/**
 * Returns a color as a tuple of integer rgb bands
 * 
 * ## Example
 * ```gleam
 * color.to_rgb_tuple(color.GleamLucy)
 * // -> #(255, 175, 243)
 * ```
 */
export function to_rgb_tuple(color) {
  if (color instanceof RGBA) {
    let r = color.r;
    let g = color.g;
    let b = color.b;
    return [r, g, b];
  } else if (color instanceof RGB) {
    let r = color.r;
    let g = color.g;
    let b = color.b;
    return [r, g, b];
  } else if (color instanceof GleamLucy) {
    return [255, 175, 243];
  } else if (color instanceof GleamNavy) {
    return [41, 45, 62];
  } else if (color instanceof Maroon) {
    return [128, 0, 0];
  } else if (color instanceof DarkRed) {
    return [139, 0, 0];
  } else if (color instanceof Brown) {
    return [165, 42, 42];
  } else if (color instanceof Firebrick) {
    return [178, 34, 34];
  } else if (color instanceof Crimson) {
    return [220, 20, 60];
  } else if (color instanceof Red) {
    return [255, 0, 0];
  } else if (color instanceof Tomato) {
    return [255, 99, 71];
  } else if (color instanceof Coral) {
    return [255, 127, 80];
  } else if (color instanceof IndianRed) {
    return [205, 92, 92];
  } else if (color instanceof LightCoral) {
    return [240, 128, 128];
  } else if (color instanceof DarkSalmon) {
    return [233, 150, 122];
  } else if (color instanceof Salmon) {
    return [250, 128, 114];
  } else if (color instanceof LightSalmon) {
    return [255, 160, 122];
  } else if (color instanceof OrangeRed) {
    return [255, 69, 0];
  } else if (color instanceof DarkOrange) {
    return [255, 140, 0];
  } else if (color instanceof Orange) {
    return [255, 165, 0];
  } else if (color instanceof Gold) {
    return [255, 215, 0];
  } else if (color instanceof DarkGoldenRod) {
    return [184, 134, 11];
  } else if (color instanceof GoldenRod) {
    return [218, 165, 32];
  } else if (color instanceof PaleGoldenRod) {
    return [238, 232, 170];
  } else if (color instanceof DarkKhaki) {
    return [189, 183, 107];
  } else if (color instanceof Khaki) {
    return [240, 230, 140];
  } else if (color instanceof Olive) {
    return [128, 128, 0];
  } else if (color instanceof Yellow) {
    return [255, 255, 0];
  } else if (color instanceof YellowGreen) {
    return [154, 205, 50];
  } else if (color instanceof DarkOliveGreen) {
    return [85, 107, 47];
  } else if (color instanceof OliveDrab) {
    return [107, 142, 35];
  } else if (color instanceof LawnGreen) {
    return [124, 252, 0];
  } else if (color instanceof Chartreuse) {
    return [127, 255, 0];
  } else if (color instanceof GreenYellow) {
    return [173, 255, 47];
  } else if (color instanceof DarkGreen) {
    return [0, 100, 0];
  } else if (color instanceof Green) {
    return [0, 128, 0];
  } else if (color instanceof ForestGreen) {
    return [34, 139, 34];
  } else if (color instanceof Lime) {
    return [0, 255, 0];
  } else if (color instanceof LimeGreen) {
    return [50, 205, 50];
  } else if (color instanceof LightGreen) {
    return [144, 238, 144];
  } else if (color instanceof PaleGreen) {
    return [152, 251, 152];
  } else if (color instanceof DarkSeaGreen) {
    return [143, 188, 143];
  } else if (color instanceof MediumSpringGreen) {
    return [0, 250, 154];
  } else if (color instanceof SpringGreen) {
    return [0, 255, 127];
  } else if (color instanceof SeaGreen) {
    return [46, 139, 87];
  } else if (color instanceof MediumAquaMarine) {
    return [102, 205, 170];
  } else if (color instanceof MediumSeaGreen) {
    return [60, 179, 113];
  } else if (color instanceof LightSeaGreen) {
    return [32, 178, 170];
  } else if (color instanceof DarkSlateGray) {
    return [47, 79, 79];
  } else if (color instanceof Teal) {
    return [0, 128, 128];
  } else if (color instanceof DarkCyan) {
    return [0, 139, 139];
  } else if (color instanceof Aqua) {
    return [0, 255, 255];
  } else if (color instanceof Cyan) {
    return [0, 255, 255];
  } else if (color instanceof LightCyan) {
    return [224, 255, 255];
  } else if (color instanceof DarkTurquoise) {
    return [0, 206, 209];
  } else if (color instanceof Turquoise) {
    return [64, 224, 208];
  } else if (color instanceof MediumTurquoise) {
    return [72, 209, 204];
  } else if (color instanceof PaleTurquoise) {
    return [175, 238, 238];
  } else if (color instanceof AquaMarine) {
    return [127, 255, 212];
  } else if (color instanceof PowderBlue) {
    return [176, 224, 230];
  } else if (color instanceof CadetBlue) {
    return [95, 158, 160];
  } else if (color instanceof SteelBlue) {
    return [70, 130, 180];
  } else if (color instanceof CornFlowerBlue) {
    return [100, 149, 237];
  } else if (color instanceof DeepSkyBlue) {
    return [0, 191, 255];
  } else if (color instanceof DodgerBlue) {
    return [30, 144, 255];
  } else if (color instanceof LightBlue) {
    return [173, 216, 230];
  } else if (color instanceof SkyBlue) {
    return [135, 206, 235];
  } else if (color instanceof LightSkyBlue) {
    return [135, 206, 250];
  } else if (color instanceof MidnightBlue) {
    return [25, 25, 112];
  } else if (color instanceof Navy) {
    return [0, 0, 128];
  } else if (color instanceof DarkBlue) {
    return [0, 0, 139];
  } else if (color instanceof MediumBlue) {
    return [0, 0, 205];
  } else if (color instanceof Blue) {
    return [0, 0, 255];
  } else if (color instanceof RoyalBlue) {
    return [65, 105, 225];
  } else if (color instanceof BlueViolet) {
    return [138, 43, 226];
  } else if (color instanceof Indigo) {
    return [75, 0, 130];
  } else if (color instanceof DarkSlateBlue) {
    return [72, 61, 139];
  } else if (color instanceof SlateBlue) {
    return [106, 90, 205];
  } else if (color instanceof MediumSlateBlue) {
    return [123, 104, 238];
  } else if (color instanceof MediumPurple) {
    return [147, 112, 219];
  } else if (color instanceof DarkMagenta) {
    return [139, 0, 139];
  } else if (color instanceof DarkViolet) {
    return [148, 0, 211];
  } else if (color instanceof DarkOrchid) {
    return [153, 50, 204];
  } else if (color instanceof MediumOrchid) {
    return [186, 85, 211];
  } else if (color instanceof Purple) {
    return [128, 0, 128];
  } else if (color instanceof Thistle) {
    return [216, 191, 216];
  } else if (color instanceof Plum) {
    return [221, 160, 221];
  } else if (color instanceof Violet) {
    return [238, 130, 238];
  } else if (color instanceof Magenta) {
    return [255, 0, 255];
  } else if (color instanceof Fuchsia) {
    return [255, 0, 255];
  } else if (color instanceof Orchid) {
    return [218, 112, 214];
  } else if (color instanceof MediumVioletRed) {
    return [199, 21, 133];
  } else if (color instanceof PaleVioletRed) {
    return [219, 112, 147];
  } else if (color instanceof DeepPink) {
    return [255, 20, 147];
  } else if (color instanceof HotPink) {
    return [255, 105, 180];
  } else if (color instanceof LightPink) {
    return [255, 182, 193];
  } else if (color instanceof Pink) {
    return [255, 192, 203];
  } else if (color instanceof AntiqueWhite) {
    return [250, 235, 215];
  } else if (color instanceof Beige) {
    return [245, 245, 220];
  } else if (color instanceof Bisque) {
    return [255, 228, 196];
  } else if (color instanceof BlanchedAlmond) {
    return [255, 235, 205];
  } else if (color instanceof Wheat) {
    return [245, 222, 179];
  } else if (color instanceof CornSilk) {
    return [255, 248, 220];
  } else if (color instanceof LemonChiffon) {
    return [255, 250, 205];
  } else if (color instanceof LightGoldenRodYellow) {
    return [250, 250, 210];
  } else if (color instanceof LightYellow) {
    return [255, 255, 224];
  } else if (color instanceof SaddleBrown) {
    return [139, 69, 19];
  } else if (color instanceof Sienna) {
    return [160, 82, 45];
  } else if (color instanceof Chocolate) {
    return [210, 105, 30];
  } else if (color instanceof Peru) {
    return [205, 133, 63];
  } else if (color instanceof SandyBrown) {
    return [244, 164, 96];
  } else if (color instanceof BurlyWood) {
    return [222, 184, 135];
  } else if (color instanceof Tan) {
    return [210, 180, 140];
  } else if (color instanceof RosyBrown) {
    return [188, 143, 143];
  } else if (color instanceof Moccasin) {
    return [255, 228, 181];
  } else if (color instanceof NavajoWhite) {
    return [255, 222, 173];
  } else if (color instanceof PeachPuff) {
    return [255, 218, 185];
  } else if (color instanceof MistyRose) {
    return [255, 228, 225];
  } else if (color instanceof LavenderBlush) {
    return [255, 240, 245];
  } else if (color instanceof Linen) {
    return [250, 240, 230];
  } else if (color instanceof OldLace) {
    return [253, 245, 230];
  } else if (color instanceof PapayaWhip) {
    return [255, 239, 213];
  } else if (color instanceof SeaShell) {
    return [255, 245, 238];
  } else if (color instanceof MintCream) {
    return [245, 255, 250];
  } else if (color instanceof SlateGray) {
    return [112, 128, 144];
  } else if (color instanceof LightSlateGray) {
    return [119, 136, 153];
  } else if (color instanceof LightSteelBlue) {
    return [176, 196, 222];
  } else if (color instanceof Lavender) {
    return [230, 230, 250];
  } else if (color instanceof FloralWhite) {
    return [255, 250, 240];
  } else if (color instanceof AliceBlue) {
    return [240, 248, 255];
  } else if (color instanceof GhostWhite) {
    return [248, 248, 255];
  } else if (color instanceof Honeydew) {
    return [240, 255, 240];
  } else if (color instanceof Ivory) {
    return [255, 255, 240];
  } else if (color instanceof Azure) {
    return [240, 255, 255];
  } else if (color instanceof Snow) {
    return [255, 250, 250];
  } else if (color instanceof Black) {
    return [0, 0, 0];
  } else if (color instanceof DimGray) {
    return [105, 105, 105];
  } else if (color instanceof DimGrey) {
    return [105, 105, 105];
  } else if (color instanceof Gray) {
    return [128, 128, 128];
  } else if (color instanceof Grey) {
    return [128, 128, 128];
  } else if (color instanceof DarkGray) {
    return [169, 169, 169];
  } else if (color instanceof DarkGrey) {
    return [169, 169, 169];
  } else if (color instanceof Silver) {
    return [192, 192, 192];
  } else if (color instanceof LightGray) {
    return [211, 211, 211];
  } else if (color instanceof LightGrey) {
    return [211, 211, 211];
  } else if (color instanceof Gainsboro) {
    return [220, 220, 220];
  } else if (color instanceof WhiteSmoke) {
    return [245, 245, 245];
  } else {
    return [255, 255, 255];
  }
}

/**
 * Returns a color as a list of integer rbg or rbga bands
 * 
 * ## Example
 * ```gleam
 * color.to_bands(color.GleamLucy)
 * // -> [255, 175, 243]
 * ```
 */
export function to_bands(color) {
  if (color instanceof RGBA) {
    let r = color.r;
    let g = color.g;
    let b = color.b;
    let a = color.a;
    return toList([r, g, b, a]);
  } else {
    let c = color;
    let $ = to_rgb_tuple(c);
    let r;
    let g;
    let b;
    r = $[0];
    g = $[1];
    b = $[2];
    return toList([r, g, b]);
  }
}

/**
 * Returns a color as a tuple of integer rgba bands
 * 
 * ## Example
 * ```gleam
 * color.to_rgba_tuple(color.GleamLucy)
 * // -> #(255, 175, 243, 255)
 * ```
 */
export function to_rgba_tuple(color) {
  if (color instanceof RGBA) {
    let r = color.r;
    let g = color.g;
    let b = color.b;
    let a = color.a;
    return [r, g, b, a];
  } else {
    let c = color;
    let $ = to_rgb_tuple(c);
    let r;
    let g;
    let b;
    r = $[0];
    g = $[1];
    b = $[2];
    return [r, g, b, 255];
  }
}

/**
 * Adds an alpha band of the given value to a color, possibly making it
 * partially transparent.
 * 
 * ## Example
 * ```gleam
 * color.add_alpha_band(color.GleamLucy, of: 128)
 * |> color.to_rgba_tuple
 * // -> #(255, 175, 243, 128)
 * ```
 */
export function add_alpha_band(color, alpha) {
  let $ = to_rgb_tuple(color);
  let r;
  let g;
  let b;
  r = $[0];
  g = $[1];
  b = $[2];
  return new RGBA(r, g, b, alpha);
}
