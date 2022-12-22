with AI302.Containers.Hashed_Maps;
pragma Elaborate_All (AI302.Containers.Hashed_Maps);

with Hash_Integer;

package Integer_Character_Maps is
   new AI302.Containers.Hashed_Maps (Integer, Character, Hash_Integer);

pragma Preelaborate (Integer_Character_Maps);

