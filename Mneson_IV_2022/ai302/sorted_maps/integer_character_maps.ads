with Sorted_Maps;
pragma Elaborate_All (Sorted_Maps);

package Integer_Character_Maps is
   new Sorted_Maps (Integer, Character);

pragma Preelaborate (Integer_Character_Maps);

