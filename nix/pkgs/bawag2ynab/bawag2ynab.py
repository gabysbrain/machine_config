#! /usr/bin/env python

from sys import argv
import os
from glob import glob
from getpass import getpass
import csv

BAWAG_BASE = 'https://ebanking.bawagpsk.com'
BAWAG_LOGIN = BAWAG_BASE + '/InternetBanking/InternetBanking?d=login&svc=BAWAG&ui=html&lang=de'
BAWAG_ACCT = BAWAG_BASE + '/InternetBanking/InternetBanking/0724734420$'

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options

options = Options()
options.set_preference("browser.download.folderList",2)
options.set_preference("browser.download.manager.showWhenStarting", False)
options.set_preference("browser.download.dir","/Downloads")
options.set_preference("browser.helperApps.neverAsk.saveToDisk", "text/csv")

def bawag_download(username, password):
  driver = webdriver.Firefox(firefox_options=options)
  #driver = webdriver.Chrome(chrome_options=options, executable_path="/run/current-system/sw/bin/chromium")
  #driver = webdriver.Firefox()
  try:
    driver.get(BAWAG_LOGIN)

    driver.find_element_by_xpath(r'//form//input[@name="dn"]').send_keys(username)
    driver.find_element_by_xpath(r'//form//input[@name="pin"]').send_keys(password)
    button = driver.find_element_by_xpath(r'//form//div[@class="submit-button"]/button').click()

    driver.implicitly_wait(10) # seconds
    #driver.get(BAWAG_ACCT)
    r = driver.find_element_by_xpath(r'/html/body/div[1]/div[6]/div[2]/form[2]/ul/li/div/table/tbody/tr[1]/td[1]/a').click()
    element = WebDriverWait(driver, 10).until(
          EC.presence_of_element_located((By.CLASS_NAME, "btn-csv"))
          #EC.presence_of_element_located((By.XPATH, "/html/body/div[1]/div[6]/div[2]/form[2]/div[3]/div/a[2]"))
    )
    element.click()
    driver.implicitly_wait(5) # seconds
  finally:
    driver.close()

def find_download():
  files = glob('/home/torsnet6cs/Downloads/BAWAG_*.csv')
  print(files)
  fmtimes = [(f, os.stat(f).st_mtime) for f in files]
  fmtimes.sort(key=lambda x: x[1], reverse=True)
  return fmtimes[0][0]

def get_login():
  uname = raw_input('username: ')
  #uname = input('username: ')
  pword = getpass('password: ')
  return uname, pword

# process a transaction into YNAB's budget format
def proc_trans(trans):
  # things YNAB needs
  date = trans[2]
  payee = '"' + trans[1] + '"'
  memo = '"' + trans[1] + '"'
  amt = trans[4]

  # adjust the date format
  dd, mm, yyyy = date.split('.')
  date = mm + "/" + dd + "/" + yyyy
  # split the cash
  inflow = None
  outflow = None
  if amt[0] == '+':
    inflow = amt[1:]
  else:
    outflow = amt[1:]
  return (date, payee, memo, outflow, inflow)

# https://docs.youneedabudget.com/article/921-importing-a-csv-file
if __name__ == '__main__':
  #uname, pword = get_login()
  #bawag_download(uname, pword)
  infile_name = find_download()
  outfile_name = './ynab.csv'

  with open(infile_name) as infile:
    with open(outfile_name, 'w') as outfile:
  # with open(infile_name, newline='', encoding='latin-1') as infile:
    # with open(outfile_name, 'w', newline='', encoding='utf-8') as outfile:
      transactions = csv.reader(infile, delimiter=';')
      ynab = csv.writer(outfile, delimiter=',', quotechar='"')
      ynab.writerow(['Date','Payee','Memo','Outflow','Inflow'])
      for trans in transactions:
        date, payee, memo, outflow, inflow = proc_trans(trans)
        ynab.writerow([date, payee, memo, outflow, inflow])



