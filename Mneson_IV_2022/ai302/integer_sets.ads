with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

package Integer_Sets is
   new AI302.Containers.Sorted_Sets (Integer);

pragma Preelaborate (Integer_Sets);

