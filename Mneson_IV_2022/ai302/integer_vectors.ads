with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

package Integer_Vectors is
   new AI302.Containers.Indefinite_Vectors (Natural, Integer);

pragma Preelaborate (Integer_Vectors);
