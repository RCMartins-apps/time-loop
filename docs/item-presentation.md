# Renaming and reskinning items

Edit `src/main/scala/pt/rcmartins/loop/model/ItemCatalog.scala` to change an item's public name, SVG icon path, or icon color. Each entry is keyed by the existing numeric item ID. For example, cooked Momo is item 10 and Frozen Momo is item 7; either can become a more generic food without changing recipes or saves.

Gameplay code refers to `ItemType` objects. Generated action titles, inventory labels, reward descriptions, and recipe ingredients resolve names from those objects. Typed descriptions carry the item reference to the renderer, which gets the icon and color from the same catalog. Do not paste public item names into new action text; interpolate `item.name`, or use a typed `EffectLabel` for rewards/costs.

Aliases in the catalog are recognition-only wording for free-form/custom text and previously saved narrative. They do not determine an item's public name. Most descriptions use direct typed references; free-form story text can use the canonical name or a catalog alias for inline icons. Previously saved story sentences remain historical text rather than being rewritten when an item is renamed.

Keep numeric item IDs and action IDs unchanged. Existing internal identifiers such as `ItemType.Momo` describe stable gameplay identities, not the public wording. Renaming those identifiers is optional and does not change what players see.
