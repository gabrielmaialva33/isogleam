import { toList } from "../../gleam.mjs";

/**
 * Aspect ratios válidos
 */
export const valid_aspect_ratios = /* @__PURE__ */ toList([
  "21:9",
  "16:9",
  "3:2",
  "4:3",
  "5:4",
  "1:1",
  "4:5",
  "3:4",
  "2:3",
  "9:16",
]);

/**
 * Prompt completo pra geração de building isométrico
 * SEGREDO: especificações exatas de ângulo e composição
 */
export function building_prompt(description) {
  return ("Highly detailed realistic isometric building render: " + description) + ".\nStyle: Professional isometric city-builder game asset, 45-degree isometric perspective, photorealistic materials and textures, intricate architectural details like windows, doors, roof tiles, bricks, and trim work. Similar quality to Cities Skylines or SimCity 4 building assets.\nCRITICAL PROJECTION: Use TRUE ISOMETRIC DIMETRIC PROJECTION with EXACTLY 26.565-degree vertical angle. The ground plane MUST be at precisely 30-degree angles from horizontal (left and right edges). All horizontal lines must be parallel and at 30-degree angles. This is non-negotiable for game asset compatibility.\nCRITICAL COMPOSITION: The building must be PERFECTLY CENTERED both horizontally and vertically in the image. The building's center axis must align exactly with the image center. Equal empty space on left and right sides. The building base should be centered at the bottom third of the image.\nCRITICAL FRAMING: The ENTIRE building must be fully visible from base to rooftop with NO cropping. Minimize padding - leave only THIN margins (5-10% of image size) on all sides. The building should fill most of the frame while still being completely visible. Scale to maximize size within bounds. Ensure the building and its base do NOT extend beyond the frame or create visual overflow.\nCRITICAL BASE REQUIREMENT: The building MUST include a ground-level base/platform/plot that is FLAT and perfectly aligned with the isometric ground plane. The base should occupy 25-35% of the image height and include pavement, plaza tiles, sidewalk, grass patches, small landscaping, or decorative ground details. The base edges must be STRAIGHT and follow the 30-degree isometric angles exactly. Match the base style to building architecture (modern concrete plaza for glass towers, brick pavement for historic buildings, grass/gardens for residential). The base provides the footprint - the building structure should be contained WITHIN the base boundaries.\nTechnical: Sharp clean render, studio lighting, pure white or light gray solid background, single isolated building with its ground plot, no additional shadows cast outside the asset, no other buildings or objects.\nQuality: Ultra high detail, realistic proportions, natural color palette, visible textures (glass, metal, stone, brick, pavement, grass), professional 3D render quality matching game asset standards.";
}

/**
 * Prompt pra pixel art estilo SimCity 2000 (nossa adaptação)
 */
export function pixelart_prompt(description) {
  return ("Isometric pixel art building: " + description) + ".\nStyle: SimCity 2000, RollerCoaster Tycoon 2, 16-bit graphics, limited color palette (64 colors max), visible dithering patterns.\nCRITICAL PROJECTION: TRUE ISOMETRIC with 26.565-degree vertical angle, 30-degree ground plane angles.\nCRITICAL: Pixel-perfect edges, NO anti-aliasing, NO smooth gradients.\nCRITICAL BASE: Flat isometric ground tile, grass or pavement, aligned to pixel grid.\nBackground: Pure magenta (#FF00FF) for easy removal.\nQuality: Clean pixel art, consistent pixel size (no sub-pixel), retro game aesthetic.";
}

/**
 * Prompt pra análise de aspect ratio (DeepSeek local)
 */
export function aspect_ratio_prompt(description) {
  return ("Analyze this building description and determine the best aspect ratio:\n\n\"" + description) + "\"\n\nConsider the building's natural proportions:\n- Very tall buildings (skyscrapers, towers, monuments) → \"9:16\" or \"2:3\" (tall portrait)\n- Tall buildings (office buildings, apartments, churches) → \"3:4\" or \"4:5\" (portrait)\n- Square/balanced buildings (houses, shops, small offices, parks) → \"1:1\" (square)\n- Wide buildings (warehouses, malls, factories) → \"5:4\" or \"4:3\" (landscape)\n- Very wide buildings (stadiums, airports, hangars) → \"3:2\" or \"16:9\" (wide landscape)\n\nReturn ONLY ONE of these aspect ratio strings: \"21:9\", \"16:9\", \"3:2\", \"4:3\", \"5:4\", \"1:1\", \"4:5\", \"3:4\", \"2:3\", \"9:16\"\nNo explanation, just the ratio string.";
}

/**
 * Prompt pra análise de atributos do building (DeepSeek local com visão)
 */
export function analyze_prompt() {
  return "Analyze this building sprite and determine its game attributes.\n\nReturn a JSON object with these exact fields:\n{\n  \"category\": \"residential\" or \"commercial\" or \"industrial\" or \"service\" or \"recreation\",\n  \"size\": 1 for small, 2 for medium, 3 for large, 4 for massive,\n  \"maxPop\": population capacity (0 for non-residential, 5-300 for residential),\n  \"maxJobs\": jobs provided (0 for residential, 10-200 for others),\n  \"pollution\": -25 to 55 (negative for parks, positive for factories),\n  \"landValue\": -20 to 80 (effect on nearby property values),\n  \"suggestedCost\": 100-15000 (construction cost),\n  \"suggestedName\": \"Short 2-4 word name\"\n}\n\nIMPORTANT: Return ONLY the JSON object, nothing else.";
}
