# Sonnets from the Portuguese 

## Menu

> [Author](#author_heading)
> [Part I](#part1_heading)
> [Part VI](#part6_heading)
> [Part XX](#part20_heading)
> [Part XXIII](#part23_heading)
> [Part XXXIII](#part33_heading)
> [Part XLIII](#part43_heading)
> [Code Block](#codeblock_heading)
___________________________________

## Author {#author_heading}

The author is [Elizabeth Barrett Browning](https://en.wikipedia.org/wiki/Elizabeth_Barrett_Browning "A brief Introduction to Browning"), an English poet of the Victorian era.


![Picture of her: ](https://upload.wikimedia.org/wikipedia/commons/6/68/Elizabeth_Barrett_Browning.jpg "Elizabeth Barrett Browning")

***

## Part I - Paragraph {#part1_heading}

#### First Paragraph

**I thought once how Theocritus had sung**   
~~Of the sweet years, the dear and wished-for years~~,  
*Who each one in a gracious hand appears*  
To bear a gift for mortals, ==*old or young*==:  
And, as I mused it in his antique tongue,  
I saw, in gradual vision through my tears,  
***The sweet, sad years, the melancholy years,***  

:joy:

#### Second Paragraph

Those of my own life, who by turns had flung  
A shadow across me. Straightway I was 'ware,  
So weeping, how a *mystic Shape* did move  
***B~e~hind m^e^***, and drew me backward by the hair;  
And a voice said in mastery, while I strove, --  
`Guess now who holds thee?` -- `Death.` I said. But, there  
The silver answer rang, -- **`Not Death, but love.`**  

:smily:

---

## Part VI - Block Quote {#part6_heading}

#### First Quote 

> ***Go from me.  Yet I feel that I shall stand***
> ~~Henceforward in thy shadow.  Nevermore~~
> *Alone upon the threshold of my door*
> ==Of individual life, I shall command==
> `The uses of my soul, nor lift my hand`
> **Serenely in the sunshine as before,**
> Without the sense of that which I forbore—

#### Second Quote

> Thy touch upon the palm.  The widest land
> Doom takes to part us, leaves thy heart in mine
> With pulses that beat double.  What I do
> And what I dream include thee, as the wine
> Must taste of its own grapes.  And when I sue
> God for myself, He hears that name of thine,
> And sees within my eyes the tears of two.

___

## Part XXXIII - Unordered Lists    {#part33_heading}

- **Yes, call me by my pet-name! let me hear**
- ~~The name I used to run at, when a child,~~
  - From innocent play, and leave the cowslips piled,
  - To glance up in some face that proved me dear  
    - With the look of its eyes. I miss the clear
      - Fond voices, which, being drawn and reconciled
        - *Into the ==**music of Heaven's**== undefiled,*
        - Call me no longer. Silence on the bier,
        - While I *call God*...**call God!**—So let thy mouth
          - Be heir to those who are now ==e^x^animat~e~==:
            - Gather the ==*north flowers to complete the south*==,
              - And catch the early love up in the late!
                - Yes, call me by that name,—and I, in truth,
                - With the same heart, will answer, and not wait. 
********

## Part XLIII - Ordered Lists     {#part43_heading}

1. **How do I love thee?  Let me count the ways.**
2. *I love thee to the depth and breadth and height*
3. My soul can reach, when feeling out of sight
4. For the ends of Being and ideal Grace.
  5. I love thee to the level of ==e^ve^ryday’s==
     11. Most quiet need, by sun and candlelight.
        8. ~~I love thee freely, as men strive for Right;~~
        3. I love thee ==pure~ly~==, as they turn from Praise.
        9. I love thee with the passion put to use
           1.  In my old griefs, and with my childhood’s faith.
           2.  I love thee with a love I seemed to lose
           3.  With my lost saints,—I love thee with the breath,
           4.  Smiles, tears, of all my life!—and, if God choose,
           5.  ==***I shall but love thee better after death.***==
---------

## Part XX - Table {#part20_heading}

| Column 1 | Column 2 | Column 3 |
| ---- | ---- | ---- |
| Belovëd, my Belovëd, when I *think* | Went counting all my chains as if that **so** | With personal act or speech,—nor ever **cull** |
| That thou wast in the world a year **ago**, | They never could fall off at any **blow** | Some prescience of thee with the blossoms ==white== |
| What time I sat alone here in the **snow** | Struck by thy possible hand,—why, thus I *drink* | Thou sawest growing!  Atheists are as **dull**, |
| And saw no footprint, heard the silence *sink* | Of life’s great cup of wonder!  **Wonderful**, | Who cannot guess God’s presence out of ==sight==. |
| No moment at thy voice, but, link by *link*, | Never to feel thee thrill the day or ==night== | **`END`** |
________

## Part XXIII - Definition List {#part23_heading}

**Is it indeed so?  If I lay here dead:**
: ~~Wouldst thou miss any life in losing mine?~~
: ==And would the sun for thee more coldly shine==

**Because of grave-damps falling round my head:**
: *I marvelled, my Belovëd, when I read*
: ==***Thy thought so in the letter.  I am thine-***==
: `But . . . so much to thee?  Can I pour thy wine`

---

## Code Block {#codeblock_heading}

#### Some Python Code

```
  import itertools

  def twoSum(self, nums, target):
      for i, n1 in enumerate(nums[ : -1]):
          for j in range(i + 1, len(nums)):
              if n1 + nums[j] == target:
                  return [i, j]
```

#### Some Java Code 

~~~
  public class LRUCache {

    public class Node {
        private int val;
        private Node left, right;

        public Node(int val) {
            this.val = val;
        }
    }

    private int capacity;

    public LRUCache(int k) {
        capacity = k;
        ...
    }

    public int get(int key) {

    }

    public void put(int key, int value) {
        if (occur.containsKey(key)) {
            // If `key` already exists, no need to evice anything since
            // the total number won't change; only need to update its value,
            // then move the node associated with `key` to the end.
            Node node = occur.get(key);
            node.val = value; // Update its value
            remove(node);
            addLast(node);
            
        } else {
            // A new `key` is passed, we need to check if we have reached `cap`
            // If so, need to evict the first (least recently used) node;
            // otherwise, no eviction is needed and simply add the new pair
            if (occur.size() == cap) {
                occur.remove(head.next.key);
                remove(head.next);
            }
            Node newNode = new Node(key, value);
            occur.put(key, newNode);
            addLast(newNode);
        }
    }
  }
~~~

#### Some Haskell Code 

```
  testMap :: Test
  testMap =
    TestList
      [ toUpperString' "abc" ~?= toUpperString "abc",
        shiftPoly' (0.5, 0.5) [(1, 1), (2, 2), (3, 3)]
          ~?= shiftPoly (0.5, 0.5) [(1, 1), (2, 2), (3, 3)]
      ]

```

______________


