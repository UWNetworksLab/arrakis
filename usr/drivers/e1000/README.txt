
NOTES:
	Driver options type:
		e1000n -h
 
	Simics:
		Currently Simics doesn't provide an EEPROM for the tested network cards and the mac_address
		argument doesn't seem to set the MAC address properly. You will have to specify it manually:
	
		e1000 mac=54:10:10:53:00:30
		
		You can add the mac address argument to your grub configuration as noted in the general section
		if you wish.


