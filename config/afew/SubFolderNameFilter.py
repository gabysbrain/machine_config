from afew.filters.BaseFilter import Filter
from afew.NotmuchSettings import notmuch_settings
from afew.FilterRegistry import register_filter
import re
import shlex

# TODO: right now this is a copy of FolderNameFilter because too many 
# private methods and variables are used and not sure how to override everything
@register_filter
class SubFolderNameFilter(Filter):
  message = 'Tags all messages in a subfolder with their folder'

  def __init__(self, database, folder_blacklist='', folder_transforms='',
               maildir_separator='.', folder_explicit_list='',
               folder_lowercases='', subfolder=''):
    super().__init__(database)

    self._filename_pattern = 'dsgjadfgasfas'
    if subfolder != '':
      self._filename_pattern = \
         '{mail_root}/{mail_subfolder}/(?P<maildirs>.*)/(cur|new)/[^/]+'.format(
            mail_root=notmuch_settings.get('database', 'path').rstrip('/'),
            mail_subfolder=subfolder)
    self._folder_explicit_list = set(shlex.split(folder_explicit_list))
    self._folder_blacklist = set(shlex.split(folder_blacklist))
    self._folder_transforms = self._parse_transforms(folder_transforms)
    self._folder_lowercases = folder_lowercases != ''
    self._maildir_separator = maildir_separator

  def handle_message(self, message):
    # Find all the dirs in the mail directory that this message
    # belongs to
    maildirs = [re.match(self._filename_pattern, filename)
                  for filename in message.get_filenames()]
    maildirs = filter(None, maildirs)
    if maildirs:
      # Make the folders relative to mail_root and split them.
      folder_groups = [maildir.group('maildirs').split(self._maildir_separator)
                       for maildir in maildirs]
      folders = set([folder
                       for folder_group in folder_groups
                       for folder in folder_group])
      self.log.debug('found folders {} for message {!r}'.format(
          folders, message.get_header('subject')))

      # remove blacklisted folders
      clean_folders = folders - self._folder_blacklist
      if self._folder_explicit_list:
        # only explicitly listed folders
        clean_folders &= self._folder_explicit_list
      # apply transformations
      transformed_folders = self._transform_folders(clean_folders)

      self.add_tags(message, *transformed_folders)

  def _transform_folders(self, folders):
    """
    Transforms the given collection of folders according to the transformation rules.
    """
    transformations = set()
    for folder in folders:
      if folder in self._folder_transforms:
        transformations.add(self._folder_transforms[folder])
      else:
        transformations.add(folder)
    if self._folder_lowercases:
      rtn = set()
      for folder in transformations:
        rtn.add(folder.lower())
      return rtn
    return transformations

  def _parse_transforms(self, transformation_description):
    """
    Parses the transformation rules specified in the config file.
    """
    transformations = dict()
    for rule in shlex.split(transformation_description):
      folder, tag = rule.split(':')
      transformations[folder] = tag
    return transformations

