#define AUTOMERGEABLE_MERGED_FILE \
	"this file is changed in master\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is automergeable\n" \
	"this file is changed in branch\n"

#define AUTOMERGEABLE_MERGED_FILE_CRLF \
	"this file is changed in master\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is automergeable\r\n" \
	"this file is changed in branch\r\n"

#define CONFLICTING_MERGE_FILE \
	"<<<<<<< HEAD\n" \
	"this file is changed in master and branch\n" \
	"=======\n" \
	"this file is changed in branch and master\n" \
	">>>>>>> 7cb63eed597130ba4abb87b3e544b85021905520\n"

#define CONFLICTING_DIFF3_FILE \
	"<<<<<<< HEAD\n" \
	"this file is changed in master and branch\n" \
	"||||||| initial\n" \
	"this file is a conflict\n" \
	"=======\n" \
	"this file is changed in branch and master\n" \
	">>>>>>> 7cb63eed597130ba4abb87b3e544b85021905520\n"

#define CONFLICTING_UNION_FILE \
	"this file is changed in master and branch\n" \
	"this file is changed in branch and master\n"

#define CONFLICTING_RECURSIVE_F1_TO_F2 \
	"VEAL SOUP.\n" \
	"\n" \
	"<<<<<<< HEAD\n" \
	"PUT INTO A POT THREE QUARTS OF WATER, three onions cut small, ONE\n" \
	"=======\n" \
	"PUT INTO A POT THREE QUARTS OF WATER, three onions cut not too small, one\n" \
	">>>>>>> branchF-2\n" \
	"spoonful of black pepper pounded, and two of salt, with two or three\n" \
	"slices of lean ham; let it boil steadily two hours; skim it\n" \
	"occasionally, then put into it a shin of veal, let it boil two hours\n" \
	"longer; take out the slices of ham, and skim off the grease if any\n" \
	"should rise, take a gill of good cream, mix with it two table-spoonsful\n" \
	"of flour very nicely, and the yelks of two eggs beaten well, strain this\n" \
	"mixture, and add some chopped parsley; pour some soup on by degrees,\n" \
	"stir it well, and pour it into the pot, continuing to stir until it has\n" \
	"boiled two or three minutes to take off the raw taste of the eggs. If\n" \
	"the cream be not perfectly sweet, and the eggs quite new, the thickening\n" \
	"will curdle in the soup. For a change you may put a dozen ripe tomatos\n" \
	"in, first taking off their skins, by letting them stand a few minutes in\n" \
	"hot water, when they may be easily peeled. When made in this way you\n" \
	"must thicken it with the flour only. Any part of the veal may be used,\n" \
	"but the shin or knuckle is the nicest.\n" \
	"\n" \
	"<<<<<<< HEAD\n" \
	"This certainly is a mighty fine recipe.\n" \
	"=======\n" \
	"This is a mighty fine recipe!\n" \
	">>>>>>> branchF-2\n"

#define CONFLICTING_RECURSIVE_H1_TO_H2_WITH_DIFF3 \
	"VEAL SOUP.\n" \
	"\n" \
	"<<<<<<< HEAD\n" \
	"put into a pot three quarts of water, three onions cut small, one\n" \
	"||||||| merged common ancestors\n" \
	"<<<<<<< Temporary merge branch 1\n" \
	"Put into a pot three quarts of water, THREE ONIONS CUT SMALL, one\n" \
	"||||||| merged common ancestors\n" \
	"Put into a pot three quarts of water, three onions cut small, one\n" \
	"=======\n" \
	"PUT INTO A POT three quarts of water, three onions cut small, one\n" \
	">>>>>>> Temporary merge branch 2\n" \
	"=======\n" \
	"Put Into A Pot Three Quarts of Water, Three Onions Cut Small, One\n" \
	">>>>>>> branchH-2\n" \
	"spoonful of black pepper pounded, and two of salt, with two or three\n" \
	"slices of lean ham; let it boil steadily two hours; skim it\n" \
	"occasionally, then put into it a shin of veal, let it boil two hours\n" \
	"longer; take out the slices of ham, and skim off the grease if any\n" \
	"should rise, take a gill of good cream, mix with it two table-spoonsful\n" \
	"of flour very nicely, and the yelks of two eggs beaten well, strain this\n" \
	"mixture, and add some chopped parsley; pour some soup on by degrees,\n" \
	"stir it well, and pour it into the pot, continuing to stir until it has\n" \
	"boiled two or three minutes to take off the raw taste of the eggs. If\n" \
	"the cream be not perfectly sweet, and the eggs quite new, the thickening\n" \
	"will curdle in the soup. For a change you may put a dozen ripe tomatos\n" \
	"in, first taking off their skins, by letting them stand a few minutes in\n" \
	"hot water, when they may be easily peeled. When made in this way you\n" \
	"must thicken it with the flour only. Any part of the veal may be used,\n" \
	"but the shin or knuckle is the nicest.\n"
