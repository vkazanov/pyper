def setFlags(self, flags): self.__flags = flags

class BigTest:

	def test():
		if not self.__isValidParams(
							filterType,
							homePeripheryID,
							limit,
							lvlFrom,
							lvlTo,
							ownStartDefHourFrom,
							ownStartDefHourTo,
							extStartDefHourFrom,
							extStartDefHourTo,
							attackDay,
							ownFortLvl,
							ownProfitFactor10,
							avgBuildingLevel10,
							ownBattleCountForFort,
							electedClanDBIDs):
			account.client.responseFortPublicInfo(requestID, FSD_ERROR.INVALID_PARAMS, EMPTY_FORT_LIST)
			return

		def testInner():
			if filterType == FSD_FILTER.DEFAULT:
				ic.g_channels.send_executeDatabaseCommand(
					account, 'select', 'select_fort_by_default', requestID,
					{
						'home_periphery_id'		: homePeripheryID,
						'ext_start_def_hours' 	: extStartDefHourFrom,
						'ext_finish_def_hours'	: extStartDefHourTo,
						'own_start_def_hours'	: ownStartDefHourFrom,
						'own_finish_def_hours'	: ownStartDefHourTo,
						'attack_time'			: attackDay,
						'own_fort_level'		: ownFortLvl,
						'min_fort_level'		: lvlFrom,
						'max_fort_level'		: lvlTo,
						'own_profit_factor'		: ownProfitFactor10,
						'battle_count_for_fort'	: ownBattleCountForFort,
						'dir_count'				: fortified_regions.g_cache.maxDirections,
						'rows_count'			: limit,
					})
				return

def selectAttackInfo(self, userArg=None):
	data = {
		'clanDBID'	: self.__clan.databaseID,
	}

	db.select('sel_fort_attack_info', requesterID = self.__clan.id,
		userArg = userArg, **data)
