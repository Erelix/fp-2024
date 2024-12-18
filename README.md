# fp-2024

# Board game shop

# Lib1

<round_command> rounds up the number to two decimal spaces.

<check_shipping_command> checks if the product price is over 70. If yes, no shipping cost will be applied.

<add_command> add two product prices

<discount_command> gives a discount to a product

<buy_command> calculates the final price of the purchase

<compare_command> compares two product prices

## Recursion

### Implementing real life example:

  * Terraforming Mars Kickstarter edition: Corporate CEO
     1. Terraforming Mars
        1. Rules
        2. Game board
        3. 5 Player boards
        4. 17 Corporation Cards
        5. 208 Corporation Cards
        6. 8 refrence Cards
        7. 200 player Markers
        8. 200 Resource Markers
        9. 3 Game board Markers
        10. 9 Ocean tiles
        11. 60 Greenery/city tiles
        12. 11 Special tiles
        13. First player Marker
     2. Terraforminh Mars Big Box
        1. 3D printed tiles
          1. 24 City Tiles
          2. 40 Greenery Tiles
          3. 9 Ocean Tiles
          4. 14 Special Tiles
        2. Big Box promo cards
     3. Venus Next expansion
        1. 49 project cards
        2. 5 corporation cards
        3. Milestone tile
        4. Award tile
        5. Venus Board
        6. Venus scale marker
        7. Rules
     4. Turmoil expansion
        * ...
     6. Colonies expansion
        * ...
     5. Prelude expanion
        1. Prelude 1 expanion
          * ...
        2. Prelude 2 expanion
          * ...
     7. Ellas & Hellium map expansion
        * ...


### To transfer this to my repl, the commad would look like this:

* corporateCEOTM 224 eur [includes: baseTM 37 eur (contains: 1 rules, 1 gameBoard, 5 playerBoard, 233 cards, 401 marker, 80 tiles), bigBoxTM 114 eur (contains: 167 tile, 20 card), venusTMexp 26 eur (contains: 1 rules, 1 gameBoard, 2 tile, 54 card), turmoilTMexp 26 eur (contains: 1 rules, 1 gameBoard, 3 tile, 60 card), coloniesTMexp 26 eur (contains: 1 rules, 1 gameBoard, 5 tile, 40 card), preludeTMexp 38 eur (contains: 1 rules)[includes: prelude1TMexp (contains: 1 rules, 30 card), prelude2TMexp (contains: 1 rules, 32 card)], hellasTMexp 19 eur (contains: 1 rules, 2 gameBoard)]


### Upper command formated:
<pre>
corporateCEOTM 224 eur 
   [includes: 
      baseTM 37 eur 
         (contains: 1 rules, 1 gameBoard, 5 playerBoard, 233 cards, 401 marker, 80 tiles), 
      bigBoxTM 114 eur 
         (contains: 167 tile, 20 card), 
      venusTMexp 26 eur 
         (contains: 1 rules, 1 gameBoard, 2 tile, 54 card), 
      turmoilTMexp 26 eur 
         (contains: 1 rules, 1 gameBoard, 3 tile, 60 card), 
      coloniesTMexp 26 eur 
         (contains: 1 rules, 1 gameBoard, 5 tile, 40 card), 
      preludeTMexp 38 eur 
         (contains: 1 rules) 
            [includes: 
               prelude1TMexp 
                  (contains: 1 rules, 30 card), 
               prelude2TMexp 
                  (contains: 1 rules, 32 card) 
            ], 
      hellasTMexp 19 eur 
         (contains: 1 rules, 2 gameBoard)  
   ] 
</pre>


### Random example provided by "https://bnfplayground.pauliankline.com/":

* ellas&hellasTMexp 8.1eur (contains: 0 rules)[includes: venusTMexp 31eur (contains: 32 card)[includes: crisisTMAEexp 7.1eur (contains: 79 rules), baseTMAE 4.3eur (contains: 7 card, 8 marker, 2 rules)[includes: venusTMexp 93.68eur (contains: 5 tile)]], foundationsTMAEexp 67.9eur (contains: 64384 tile)]


### Upper command formated:
<pre>
ellas&hellasTMexp 8.1eur (contains: 0 rules) 
   [includes: 
   venusTMexp 31eur (contains: 32 card) 
      [includes: 
         crisisTMAEexp 7.1eur (contains: 79 rules), 
         baseTMAE 4.3eur (contains: 7 card, 8 marker, 2 rules) 
            [includes: 
               venusTMexp 93.68eur (contains: 5 tile) 
            ] 
      ], 
      foundationsTMAEexp 67.9eur (contains: 64384 tile) 
   ] 
</pre>

# Lib2

### Changes to BNF:

1. Removed all uneeded and inseparable add_on, component grammar. Just made it simple and not repetetive:
<component> ::= <component_name>
<add_on> ::= <add_on_name> " " <price> "eur"

2. Added "%" to <discaout> to make it unique.

3. Moved "eur" from <boardgame> and <add_on> to <price> for clarity

4. Moved <quantity> from <components> to <component>, added missing <add_on> to <product>.

5. 
   - Deleted <add_ons>, <components>, and moved <add_on>, <component> to <product>. To avoid code repetition for recursive elements.
   - Separated <boardgame> to two for clarity:
     <boardgame> ::=  <boardgame_name> " " <price>  " (contains: " <products> ")"
     <boardgame_with_addons> ::= <boardgame> "[includes: " <products> "]"

6. Added <total_command>

#### NOTE:
   The changes to BNF were made because there either were mistakes or for clarity, simplicity purposes. The main logic hasn't been changed. The examples up above are still valid.


# Lib3

### Loading, Saving
State can be saved with "save" in storage.txt. Loaded with "load".

storage.txt example:
<pre>
BEGIN
add corporateCEOTM 50.0eur (contains: ), cardSleeve 5.0eur, bigBoxTM 150.0eur (contains: 2 tile, 1 gameBoard, 5 marker) [includes: playerBoard 10.0eur, metalResource 20.0eur], 3 marker, foundationsTMAEexp 50.0eur (contains: ) [includes: ];
giveDiscount corporateCEOTM 50.0eur (contains: ) 10%;
giveDiscount cardSleeve 5.0eur 5%
END
</pre>

### Batch processing
1. To start ":paste" in cmd
2. write "BEGIN"
3. commands with ";" at the end. New command - new line.
4. write "END"
5. Ctrl-d

Batch processing example:
<pre>
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| add corporateCEOTM 99.99eur (contains: 2 tile, 1 gameBoard);
| giveDiscount corporateCEOTM 99.99eur (contains: 2 tile, 1 gameBoard) 15%;
| buy 2 corporateCEOTM 99.99eur (contains: 2 tile, 1 gameBoard);
| END
| 
New products added to the state.
Discount applied to BoardGame "corporateCEOTM" 99.99 [Component 2 "tile",Component 1 "gameBoard"].
Product bought for 169.98299999999998 eur and added to purchase history.
</pre>


### Changes to Lib2:

1. Made <check_shipping_command>, <total_command>, <compare_command> functional

2. Improved <buy_command> so that it displays for how much product has been bought.

3. Removed <round_command> as it was never used and will never be used.

4. Added parseDouble as there were problems in Property tests comparing Doubles - rounding up is random.

5. Added "instance Eq Product where" because there were problems comparing products in Property tests. Now they are compared by name.

6. Changed parseProductOrIndex to first do parseProductAsLeft then parseNumberAsIndex because there were problems with add command when adding component. Quantinty was classified as index. (Problem found by property tests)
