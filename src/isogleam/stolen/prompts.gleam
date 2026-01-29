//// Prompts roubados do IsoCity-fal (otimizados pra isometric)
//// Fonte: https://github.com/blendi-remade/isometric-city-fal

/// Prompt completo pra geração de building isométrico
/// SEGREDO: especificações exatas de ângulo e composição
pub fn building_prompt(description: String) -> String {
  "Highly detailed realistic isometric building render: " <> description <> ".
Style: Professional isometric city-builder game asset, 45-degree isometric perspective, photorealistic materials and textures, intricate architectural details like windows, doors, roof tiles, bricks, and trim work. Similar quality to Cities Skylines or SimCity 4 building assets.
CRITICAL PROJECTION: Use TRUE ISOMETRIC DIMETRIC PROJECTION with EXACTLY 26.565-degree vertical angle. The ground plane MUST be at precisely 30-degree angles from horizontal (left and right edges). All horizontal lines must be parallel and at 30-degree angles. This is non-negotiable for game asset compatibility.
CRITICAL COMPOSITION: The building must be PERFECTLY CENTERED both horizontally and vertically in the image. The building's center axis must align exactly with the image center. Equal empty space on left and right sides. The building base should be centered at the bottom third of the image.
CRITICAL FRAMING: The ENTIRE building must be fully visible from base to rooftop with NO cropping. Minimize padding - leave only THIN margins (5-10% of image size) on all sides. The building should fill most of the frame while still being completely visible. Scale to maximize size within bounds. Ensure the building and its base do NOT extend beyond the frame or create visual overflow.
CRITICAL BASE REQUIREMENT: The building MUST include a ground-level base/platform/plot that is FLAT and perfectly aligned with the isometric ground plane. The base should occupy 25-35% of the image height and include pavement, plaza tiles, sidewalk, grass patches, small landscaping, or decorative ground details. The base edges must be STRAIGHT and follow the 30-degree isometric angles exactly. Match the base style to building architecture (modern concrete plaza for glass towers, brick pavement for historic buildings, grass/gardens for residential). The base provides the footprint - the building structure should be contained WITHIN the base boundaries.
Technical: Sharp clean render, studio lighting, pure white or light gray solid background, single isolated building with its ground plot, no additional shadows cast outside the asset, no other buildings or objects.
Quality: Ultra high detail, realistic proportions, natural color palette, visible textures (glass, metal, stone, brick, pavement, grass), professional 3D render quality matching game asset standards."
}

/// Prompt pra pixel art estilo SimCity 2000 (nossa adaptação)
pub fn pixelart_prompt(description: String) -> String {
  "Isometric pixel art building: " <> description <> ".
Style: SimCity 2000, RollerCoaster Tycoon 2, 16-bit graphics, limited color palette (64 colors max), visible dithering patterns.
CRITICAL PROJECTION: TRUE ISOMETRIC with 26.565-degree vertical angle, 30-degree ground plane angles.
CRITICAL: Pixel-perfect edges, NO anti-aliasing, NO smooth gradients.
CRITICAL BASE: Flat isometric ground tile, grass or pavement, aligned to pixel grid.
Background: Pure magenta (#FF00FF) for easy removal.
Quality: Clean pixel art, consistent pixel size (no sub-pixel), retro game aesthetic."
}

/// Prompt pra análise de aspect ratio (DeepSeek local)
pub fn aspect_ratio_prompt(description: String) -> String {
  "Analyze this building description and determine the best aspect ratio:

\"" <> description <> "\"

Consider the building's natural proportions:
- Very tall buildings (skyscrapers, towers, monuments) → \"9:16\" or \"2:3\" (tall portrait)
- Tall buildings (office buildings, apartments, churches) → \"3:4\" or \"4:5\" (portrait)
- Square/balanced buildings (houses, shops, small offices, parks) → \"1:1\" (square)
- Wide buildings (warehouses, malls, factories) → \"5:4\" or \"4:3\" (landscape)
- Very wide buildings (stadiums, airports, hangars) → \"3:2\" or \"16:9\" (wide landscape)

Return ONLY ONE of these aspect ratio strings: \"21:9\", \"16:9\", \"3:2\", \"4:3\", \"5:4\", \"1:1\", \"4:5\", \"3:4\", \"2:3\", \"9:16\"
No explanation, just the ratio string."
}

/// Prompt pra análise de atributos do building (DeepSeek local com visão)
pub fn analyze_prompt() -> String {
  "Analyze this building sprite and determine its game attributes.

Return a JSON object with these exact fields:
{
  \"category\": \"residential\" or \"commercial\" or \"industrial\" or \"service\" or \"recreation\",
  \"size\": 1 for small, 2 for medium, 3 for large, 4 for massive,
  \"maxPop\": population capacity (0 for non-residential, 5-300 for residential),
  \"maxJobs\": jobs provided (0 for residential, 10-200 for others),
  \"pollution\": -25 to 55 (negative for parks, positive for factories),
  \"landValue\": -20 to 80 (effect on nearby property values),
  \"suggestedCost\": 100-15000 (construction cost),
  \"suggestedName\": \"Short 2-4 word name\"
}

IMPORTANT: Return ONLY the JSON object, nothing else."
}

/// Aspect ratios válidos
pub const valid_aspect_ratios = [
  "21:9", "16:9", "3:2", "4:3", "5:4", 
  "1:1", 
  "4:5", "3:4", "2:3", "9:16"
]
